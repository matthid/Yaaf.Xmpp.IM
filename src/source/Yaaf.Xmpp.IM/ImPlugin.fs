// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.IM

open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Runtime.Features
open Yaaf.Logging

type RosterGetResult = 
    | ViaPush
    | Roster of RosterItem list * RosterVersion option

type IImService =
    abstract SendPresence : JabberId option * PresenceProcessingType -> unit
    abstract SendMessage : JabberId * MessageData -> unit
    abstract RequestRoster : string option -> Task<RosterGetResult>
    abstract UpdateRoster : RosterItem -> Task<unit>
    abstract DeleteRosterItem : JabberId -> Task<unit>
    [<CLIEvent>] abstract MessageReceived : IEvent<MessageStanza>
    [<CLIEvent>] abstract PresenceReceived : IEvent<PresenceStanza>
    [<CLIEvent>] abstract RosterPushReceived : IEvent<RosterChange * RosterVersion option>
    
//type IMessageReceivedPlugin =
//    inherit IReceivePipelineProvider<MessageStanza>

type MessagePlugin 
    (runtimeConfig : IRuntimeConfig, stanzas : IXmlStanzaService, mgr : IXmppPluginManager, neg : INegotiationService,
     registrar : IPluginManagerRegistrar) =
    do
        //if runtimeConfig.IsServerSide then
        //    Configuration.configFail "this plugin is client only."
        if runtimeConfig.StreamType.OnClientStream then
            let streamFeatures = mgr.GetPluginService<IStreamFeatureService>()
            if not <| streamFeatures.FeatureManager.HasPluginOf<SessionFeature>() then
                Configuration.configFail "MessagePlugin (on client) requires SessionFeature to work!"
            if not <| streamFeatures.FeatureManager.HasPluginOf<RosterVerFeature>() then
                Configuration.configFail "MessagePlugin (on client) requires RosterVerFeature to work!"
        //if mgr.HasPluginOf<XmppServerDeliveryPlugin>() then
        //    Configuration.configFail "MessagePlugin requires that the XmppServerPlugin is not active!"
        //if mgr.HasPluginOf<ClientPlugin.UnknownIqResponderPlugin>() then
        //    Configuration.configFail "MessagePlugin requires that the XmppClientPlugin is not active!"

    let rosterPushReceived = Event<_>()
    let presenceReceived = Event<_>()
    let messageReceived = Event<_>()
    let rosterReceivedEvents (msgStanza:Stanza<_>) = 
        // Check from and trigger specific events
        match msgStanza.Data with 
        | RosterAction.RosterChange (change, version) ->
            if msgStanza.Header.From.IsNone || msgStanza.Header.From.Value.BareId = neg.LocalJid.BareId then
                rosterPushReceived.Trigger (change, version)
        | _ -> ()

    let stanzaReceived (stanza:Stanza) = 
        // if the stanza is addressed to us then call event
        let genericHandle name isContent parser trigger = 
            if isContent stanza then
                Log.Info (fun () -> L "handling %s element (IMClientPlugin)" name)
                let msgStanza = Stanza<_>.Create(stanza, (parser stanza))
                trigger (msgStanza)

        genericHandle "message" Parsing.isContentMessage Parsing.parseContentMessage (messageReceived.Trigger)
        genericHandle "presence" Parsing.isContentPresence Parsing.parseContentPresence (presenceReceived.Trigger)
        genericHandle "roster" Parsing.isContentRoster Parsing.parseContentRoster rosterReceivedEvents
        ()

    //let streamOpened (context:XmppContext, ()) =
    //    async {
    //        // check if we can send messages
    //        if context.Permanent.BoundJid.IsSome then
    //            if (context.ActiveSession && context.Permanent.SessionAvailable) || not context.Permanent.SessionAvailable then
    //                if not context.IMSessionOpen then // only trigger once
    //                    context.IMSessionOpen <- true
    //                    do! context.Events.TriggerIMSessionOpened ()
    //
    //        return TriggerResult.Continue
    //    } 
    //    |> Log.TraceMe

    let requestRoster (version:string option) =
        async {
            let stanza = Parsing.createRosterElement (stanzas.GenerateNextId()) (Some neg.LocalJid) None (GetRoster(version))
            let! stanza = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            
            if stanza.AsInterface.IsEmptyIqResult() then
                return ViaPush
            else
                let result = Parsing.parseContentRoster stanza
                match result with
                | RosterData (list, version) -> 
                    return Roster(list, version)
                | _ -> return failwith "roster result was expected"
        } 
        |> Log.TraceMe

    let updateRoster (item:RosterItem) =
        async {
            let id = stanzas.GenerateNextId()
            let stanza = Parsing.createRosterElement id (Some neg.LocalJid) None (RosterAction.RosterChange(SetItem(item), None))
            let! _ = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            return ()
        } 
        |> Log.TraceMe

    let deleteRosterItem (item:JabberId) =
        async {
            let id = stanzas.GenerateNextId()
            let stanza = Parsing.createRosterElement id (Some neg.LocalJid) None (RosterAction.RosterChange(DeleteItem(item), None))
            let! _ = stanzas.QueueStanzaGenericReturn stanza |> Task.await
            return ()
        } 
        |> Log.TraceMe

    let sendPresence (toJid:JabberId option, presType:PresenceProcessingType) =
        let stanza = Parsing.createPresenceElement (Some <| stanzas.GenerateNextId()) None (toJid) presType
        stanzas.QueueStanzaGeneric None stanza

    let sendMessage (target: JabberId, msg : MessageData) =
        let stanza = Parsing.createMessageElement target msg
        stanzas.QueueStanzaGeneric None stanza

    let stanzaReceivedPipeline =
        { new IRawStanzaPlugin with        
            member __.ReceivePipeline = 
                { Pipeline.empty "MessagePlugin pipeline" with
                    HandlerState = 
                        fun _ -> 
                            HandlerState.ExecuteUnhandled
                    Process =
                        fun info ->
                            async {
                                let elem = info.Result.Element
                                stanzaReceived (elem)
                            } |> Async.StartAsTaskImmediate
                } :> IPipeline<_>
        }
    do
        registrar.RegisterFor<IRawStanzaPlugin> stanzaReceivedPipeline

    interface IImService with
        member __.SendPresence (jid, typ) = sendPresence (jid, typ)
        member __.SendMessage (jid, msg) = sendMessage (jid, msg)
        member __.RequestRoster ver = requestRoster ver |> Async.StartAsTaskImmediate
        member __.UpdateRoster item = updateRoster item |> Async.StartAsTaskImmediate
        member __.DeleteRosterItem item = deleteRosterItem item |> Async.StartAsTaskImmediate
        [<CLIEvent>] member __.MessageReceived = messageReceived.Publish
        [<CLIEvent>] member __.PresenceReceived = presenceReceived.Publish
        [<CLIEvent>] member __.RosterPushReceived = rosterPushReceived.Publish

    interface IXmppPlugin with
        member __.Name = "MessagePlugin"
        member x.PluginService = Service.FromInstance<IImService, _> x


