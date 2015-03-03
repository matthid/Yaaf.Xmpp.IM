// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// Implementation of XEP-0280: Message Carbons (http://xmpp.org/extensions/xep-0280.html)
namespace Yaaf.Xmpp.IM.MessageCarbons

open Yaaf.Xmpp.XmlStanzas

[<AutoOpen>]
module StanzaExtensions =
    type IStanza with
        member x.CarbonInfo
            with get () =
                x.GetCacheData<CarbonInfo> (Parsing.tryGetCarbonItem)
        member x.AddCarbonInfo v =
                x.AddContent (Parsing.createCarbonInfoElem v) 

        member x.Private
            with get () =
                x.GetCacheData<MessageCarbonPayload> (Parsing.tryGetCarbonPrivateItem)
        member x.AddPrivate v =
                x.AddContent (Parsing.createCarbonPrivateElem v) 
        member x.IsPrivate = x.Private.IsSome

module Stanza =
    let getPrivate (s:IStanza) = s.Private
    let addPrivate (d:MessageCarbonPayload) (s:IStanza) = s.AddPrivate d

    let getCarbonInfo (s:IStanza) = s.CarbonInfo
    let addCarbonInfo (d:CarbonInfo) (s:IStanza) = s.AddCarbonInfo d

namespace Yaaf.Xmpp.IM.MessageCarbons.Server

open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.ServiceDiscovery
open Yaaf.Xmpp.IM
open Yaaf.Xmpp.IM.Server
open Yaaf.Logging
open Yaaf.Xmpp.IM.StanzaForwarding
open Yaaf.Xmpp.IM.MessageCarbons

type ICarbonService =
    abstract IsCarbonEnabled : bool

/// Implementation of http://xmpp.org/extensions/xep-0280.html (XEP-0280: Message Carbons).
/// This plugin is a server side plugin.
type MessageCarbonsPlugin 
    (serverApi : IServerApi, 
     runtimeConfig : IRuntimeConfig, registrar : IPluginManagerRegistrar,
     messageService : IImServerMessageService, disco : IDiscoService) =
    
    do disco.RegisterFeatureItem (None, { Var = "urn:xmpp:carbons:2"})

    let mutable carbonsEnabled = false

    let messageSent (msg : MessageStanza, sentTo : IXmppClient list) =
        if not carbonsEnabled then ()
        else
        // From our perspective we have sent the message, from the client perspective it was received (or will be).

        // so this is either "6. Receiving Messages to the Bare JID" or "7. Receiving Messages to the Full JID"
        if not msg.IsPrivate && msg.Data.MessageType = MessageType.Chat then
            // message is not ignored with "9. Avoiding Carbons for a single message"
            let toJid = msg.Header.To.Value
            let otherConnections = 
                serverApi.ConnectionManager.GetConnections toJid.BareJid
                |> Seq.filter (fun client -> sentTo |> Seq.exists (fun a -> a = client) |> not)

            if msg.Header.To.Value.Localpart.IsNone then
                // it is bare JID, so fork it
                for con in otherConnections do
                    let stanzas = con.GetService<IXmlStanzaService>()
                    stanzas.QueueStanzaGeneric None (msg.WithHeader { msg.Header with To = Some con.RemoteJid })
            else
                // fulljid, so wrap it
                let wrapmsg =
                    Parsing.createMessageElement toJid.BareJid
                        { MessageData.Body = []; MessageData.MessageType = MessageType.Chat; 
                          MessageData.Subject = []; MessageData.Thread = None }
                
                let wrapmsg =
                    wrapmsg.AddCarbonInfo 
                        { CarbonInfo.ForwardType = CarbonType.Received; 
                          CarbonInfo.Forwarded =
                            { ForwardInfo.Delay = None
                              ForwardInfo.Xmlns = runtimeConfig.StreamNamespace
                              ForwardInfo.Inner = ForwardPayload.MessageForward msg} }
                for con in otherConnections do
                    let stanzas = con.GetService<IXmlStanzaService>()
                    stanzas.QueueStanza None 
                        (wrapmsg.WithHeader { msg.Header with To = Some con.RemoteJid; From = Some con.RemoteJid.BareJid }).AsSimple
        ()

    let handleMessageReceived (msg:MessageStanza) = 
        if not carbonsEnabled then ()
        else
        // From our perspective we have received the message, from the client perspective it was sent (or will be).

        // Make sure this is in fact a client and no private message
        if runtimeConfig.StreamType.OnClientStream && not msg.IsPrivate && msg.Data.MessageType = MessageType.Chat then
            let fullFrom = msg.Header.From.Value
            let otherConnections = serverApi.ConnectionManager.GetConnections msg.BareFrom |> Seq.filter (fun con -> con.RemoteJid <> fullFrom)
            let wrapmsg =
                Parsing.createMessageElement msg.BareFrom
                    { MessageData.Body = []; MessageData.MessageType = MessageType.Chat; 
                        MessageData.Subject = []; MessageData.Thread = None }
                
            let wrapmsg =
                wrapmsg.AddCarbonInfo 
                    { CarbonInfo.ForwardType = CarbonType.Sent; 
                        CarbonInfo.Forwarded =
                          { ForwardInfo.Delay = None
                            ForwardInfo.Xmlns = runtimeConfig.StreamNamespace
                            ForwardInfo.Inner = ForwardPayload.MessageForward msg} }
            for con in otherConnections do
                let stanzas = con.GetService<IXmlStanzaService>()
                stanzas.QueueStanza None 
                    (wrapmsg.WithHeader { msg.Header with To = Some con.RemoteJid; From = Some con.RemoteJid.BareJid }).AsSimple  

        ()

    let messageReceived =
        { new ILocalMessageReceiveStanzaPlugin with
            member __.ReceivePipeline = 
                { Pipeline.empty "Message Received Pipeline (MessageCarbons)"  with
                    HandlerState = 
                        fun _ -> 
                            HandlerState.ExecuteUnhandled
                    Process =
                        fun info -> 
                            async {
                                let elem = info.Result.Element
                                handleMessageReceived elem
                            } |> Async.StartAsTaskImmediate
                } :> IPipeline<_>
        }

    do messageService.MessageLocalSending |> Event.add messageSent
    do registrar.RegisterFor<ILocalMessageReceiveStanzaPlugin> messageReceived

    interface ICarbonService with
        member __.IsCarbonEnabled = carbonsEnabled

    interface IXmppPlugin with
        member __.Name = "ImServerMessagePlugin"
        member x.PluginService = Service.FromInstance<ICarbonService, _> x