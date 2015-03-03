// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.IM.Server

open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.ServiceDiscovery
open Yaaf.Xmpp.IM
open Yaaf.Logging


/// Offline messages are in this context messages which are currently not delivered
type IOfflineMessageStorage =
    /// Stores a message for the given user
    abstract member StoreOfflineMessage : JabberId * MessageStanza -> System.Threading.Tasks.Task
    /// Retrieves all stored offline messages for the given user
    abstract member RetrieveOfflineMessages : JabberId -> System.Threading.Tasks.Task<MessageStanza seq>

/// A resource sending strategie for messages
type ResourceSendingStrategy =
    /// Send messages to all non negative resources
    | NonNegativeResources
    /// Send messages only to the nonnegative resource with the highest priority
    | TopNonNegativeResource
    /// Send ALL messages only to the nonnegative resource with the highest priority (even when addressed to another resource).
    /// NOTE: This configuration is not XMPP.IM compliant
    | AllToTopNonNegativeResource
    /// Send ALL messages to all non negative resources (even when addressed to another resource).
    /// NOTE: This configuration is not XMPP.IM compliant
    | AllToNonNegativeResources

/// User configuration for messages.
type ImMessageUserConfig =
    {
        /// <summary> The users choosen <see cref='ResourceSendingStrategy' /></summary>
        ResourceSendingStrategy : ResourceSendingStrategy
        /// Enable offline messages for the current user
        OfflineMessagesEnabled : bool
    }

/// Manager of the user configurations
type IUserMessageUserConfig =
    /// Gets the current configuration of the given user when the message is from the given Jid
    abstract member GetConfig : toJid:JabberId * fromJid:JabberId option -> System.Threading.Tasks.Task<ImMessageUserConfig>

/// <summary>The configuration for the <see cref='IMServerMessagePlugin' /> plugin</summary>
type IImServerMessagePluginConfig =
    /// The offline message storage (None to disable)
    abstract OfflineStorage : IOfflineMessageStorage option
    /// The user configuration manager
    abstract UserConfig : IUserMessageUserConfig
/// <summary>The configuration for the <see cref='IMServerMessagePlugin' /> plugin</summary>
type ImServerMessagePluginConfig =
    {
        /// The offline message storage (None to disable)
        OfflineStorage : IOfflineMessageStorage option
        /// The user configuration manager
        UserConfig : IUserMessageUserConfig
    } with
    interface IImServerMessagePluginConfig with
        member x.OfflineStorage = x.OfflineStorage
        member x.UserConfig = x.UserConfig
    /// Convert the interface to the record
    static member OfInterface (x:IImServerMessagePluginConfig) =
        {
            OfflineStorage = x.OfflineStorage
            UserConfig = x.UserConfig
        }
    /// Default configuration
    static member Default =
        {
            OfflineStorage = None
            UserConfig = 
                { new IUserMessageUserConfig with
                    member __.GetConfig (_, _) = Task.FromResult { ResourceSendingStrategy = NonNegativeResources; OfflineMessagesEnabled = true } }
        }
/// Plugin for the LocalMessageReceive pipeline (server received a stanza that has to be processed locally)
type ILocalMessageReceiveStanzaPlugin =
    inherit IReceivePipelineProvider<MessageStanza>
type ILocalMessageSendStanzaPlugin =
    inherit ISendPipelineProvider<MessageStanza>
type IRemoteMessageSendStanzaPlugin =
    inherit ISendPipelineProvider<MessageStanza>

type IImServerMessageService =
    abstract member MessageLocalSending : IEvent<MessageStanza * IXmppClient list>

/// Handling of messages specified by XMPP.IM and http://xmpp.org/extensions/xep-0160.html
type IMServerMessagePlugin 
    (serverApi : IServerApi, neg : INegotiationService, 
     addressing : IAddressingService, stanzas : IXmlStanzaService,
     runtimeConfig : IRuntimeConfig, config : IImServerMessagePluginConfig, registrar : IPluginManagerRegistrar,
     imService : IImServerService, disco : IDiscoService) =
     
    let messageSendManager = registrar.CreateManagerFor<ILocalMessageSendStanzaPlugin>()
    let messageRemoteSendManager = registrar.CreateManagerFor<IRemoteMessageSendStanzaPlugin>()
    let messageReceiveManager = registrar.CreateManagerFor<ILocalMessageReceiveStanzaPlugin>()
    let messageLocalSending = Event<_>()
    do
        assert not (obj.ReferenceEquals(config.UserConfig, null))
        imService.StatusChanged.Add (fun (_, isInitial) -> 
            if isInitial then
                match config.OfflineStorage with
                | Some storage ->
                    async {
                        let! storedMsgs = storage.RetrieveOfflineMessages neg.RemoteJid |> Task.await
                        for msg in storedMsgs do
                            stanzas.QueueStanzaGeneric None msg
                    } 
                    |> Async.StartImmediate
                | None -> ())
        disco.RegisterFeatureItem (None, { Var = "msgoffline"})

    let handleLocalMessage (elem:MessageStanza) =
        async {
            let target = elem.Header.To.Value
            let source = elem.Header.From
            let bareTarget = target.BareJid

            let! userconfig = config.UserConfig.GetConfig (bareTarget, source) |> Task.await
            let filterClients toAll (clients:IXmppClient seq) =
                let clients =
                    clients
                        |> Seq.filter (fun (c) -> not c.IsClosed) 
                        |> Seq.map (fun c -> c, c.GetService<IImServerService>())
                        |> Seq.filter (fun (_, im) -> im.AvailableResource) 
                        |> Seq.map (fun (c, im) -> c, im, match im.ForcePresenceData().Priority with None -> 0y | Some p -> p)
                        |> Seq.filter (fun (_, _, p) -> p > -1y)
                    
                match toAll with
                | true -> clients
                | false -> if clients |> Seq.isEmpty then Seq.empty else Seq.singleton (clients |> Seq.maxBy (fun (_,_,p) -> p))
                |> Seq.map (fun (c, _, _) -> c)

            let sendClients = 
                let toAll =
                    match userconfig.ResourceSendingStrategy with
                    | NonNegativeResources
                    | AllToNonNegativeResources -> true
                    | TopNonNegativeResource
                    | AllToTopNonNegativeResource -> false
        
                match userconfig.ResourceSendingStrategy with
                | NonNegativeResources
                | TopNonNegativeResource -> // try first with target and than again with all
                    let clients = serverApi.ConnectionManager.GetConnections target |> filterClients toAll
                    if clients |> Seq.isEmpty then // handle like it was sent to bare-jid
                        serverApi.ConnectionManager.GetConnections bareTarget |> filterClients toAll
                    else clients
                | AllToTopNonNegativeResource
                | AllToNonNegativeResources -> // try directly with bareId all
                    serverApi.ConnectionManager.GetConnections bareTarget |> filterClients toAll
                |> Seq.toList

            messageLocalSending.Trigger(elem, sendClients)

            if sendClients |> Seq.isEmpty then
                // target is offline
                match config.OfflineStorage with
                | Some storage when userconfig.OfflineMessagesEnabled ->
                    // storage available and enabled by the user
                    try
                        do! storage.StoreOfflineMessage(bareTarget, elem) |> Task.awaitPlain
                    with exn ->
                        Log.Err (fun () -> L "Error while storing offline message: %O" exn)
                        let elem = StanzaException.Create(StanzaErrorType.Cancel, StanzaErrorConditon.ServiceUnavailable, elem)
                        stanzas.QueueStanzaGeneric None (elem.ErrorStanza)
                | _ ->
                    // no offline storage available or not enabled by user
                    // return service-unavailable
                    let elem = StanzaException.Create(StanzaErrorType.Cancel, StanzaErrorConditon.ServiceUnavailable, elem)
                    stanzas.QueueStanzaGeneric None (elem.ErrorStanza)
                    ()
            else
                for (c) in sendClients do
                    let stanzas = c.GetService<IXmlStanzaService>()
                    stanzas.QueueStanzaGeneric None elem
                ()
                
        }
    let handleServerMessage _ =
        async {
            // messages addressed to the server directly
            return ()
        }

    let handleMessage (elem:MessageStanza) =
        async {
            // receiving
            let! res = Pipeline.processManager "MessageReceiveManager" messageReceiveManager elem
            let elem = res.ResultElem
            if res.IsIgnored then
                Log.Warn (fun _ -> L "message was ignored: %O" elem)
            else
                // delivering
                if elem.Header.To.IsNone || serverApi.IsLocalJid elem.Header.To.Value then
                    // Stanza has to be handled by us
                    let! res = Pipeline.processSendManager "MessageSendManager" messageSendManager elem
                    let elem = res.ResultElem
                    if res.IsIgnored then
                        Log.Warn (fun _ -> L "message was ignored while delivering locally: %O" elem)
                    else
                        if addressing.IsLocalStanzaOnServer(serverApi, elem) then
                            do! handleServerMessage elem
                        else
                            do! handleLocalMessage elem
                        ()
                else
                    // deliver stanza to next server or componenet
                    let! res = Pipeline.processSendManager "MessageRemoteSendManager" messageRemoteSendManager elem
                    let elem = res.ResultElem
                    if res.IsIgnored then
                        Log.Warn (fun _ -> L "message was ignored while delivering remotely: %O" elem)
                    else
                        let raw = elem.SimpleStanza
                        do! XmppServerDeliveryPlugin.DeliverStanza (serverApi, neg.RemoteJid, runtimeConfig.StreamType, stanzas.QueueStanza None, raw)
                        ()
        }

    let messageStanzaDefaultRoutePlugin =
        { new IRawMessageStanzaPlugin with
            member __.ReceivePipeline = 
                { Pipeline.empty "ImServerPlugin Message Pipeline"  with
                    HandlerState = 
                        fun _ -> 
                            //let msgStanza :Stanza<_> = info.Result.Element
                            if neg.NegotiationCompleted then
                                // We can at least route it
                                HandlerState.ExecuteIfUnhandled 99
                            else HandlerState.Unhandled
                    Process =
                        fun info -> 
                            async {
                                let elem = info.Result.Element
                                do! handleMessage elem
                            } |> Async.StartAsTaskImmediate
                } :> IPipeline<_>
        }

    do
        registrar.RegisterFor<IRawMessageStanzaPlugin> messageStanzaDefaultRoutePlugin



    interface IImServerMessageService with
        member __.MessageLocalSending = messageLocalSending.Publish

    interface IXmppPlugin with
        member __.Name = "ImServerMessagePlugin"
        member x.PluginService = Service.FromInstance<IImServerMessageService, _> x

    /// Wraps the raw offlineStore to ignore the messages specified in XEP-0160
    static member MakeIgnoringOfflineStore (rawOfflineStore:IOfflineMessageStorage) =
        let isOnlyChatState (msg:MessageStanza) =
            false
        { new IOfflineMessageStorage with
            member x.StoreOfflineMessage(jid, msg) =
                async {
                    match msg.Data.MessageType with
                    | MessageType.Groupchat | MessageType.Headline | MessageType.Error | MessageType.Chat when isOnlyChatState msg ->
                        ()
                    | _ ->
                        do! rawOfflineStore.StoreOfflineMessage(jid, msg) |> Task.awaitPlain
                }
                |> Async.StartAsTaskImmediate
                :> System.Threading.Tasks.Task
            member x.RetrieveOfflineMessages (jid) =
                rawOfflineStore.RetrieveOfflineMessages(jid)}
