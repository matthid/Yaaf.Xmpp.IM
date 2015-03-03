// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.IM.Server

open Yaaf.Helper

open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.IM

open Yaaf.Logging


/// This type represents the storage of an roster, most methods MUST ignore the resourcepart of the JabberId (read 'XMPP IM' rfc6121 for details).
/// This behavior tested with unit tests.
type IUserRoster = 

    // Roster
    /// Version of the roster
    abstract member GetCurrentRosterVersion : unit -> RosterVersion Task
    abstract member GetRosterSize : unit -> Task<int> 

    abstract member UpdateOrAddRosterItem :  bool-> RosterItem -> Task<RosterVersion>
    abstract member GetItems : unit -> Task<RosterItem list>
    abstract member GetItem : JabberId -> Task<RosterItem option>
    abstract member DeleteItem : JabberId -> RosterVersion Task
    abstract member ChangesSince : RosterVersion -> Task<(RosterChange * RosterVersion) list>

    // presence

    /// They should be unique by FROM address (it doesn't matter if we store the last or the first)
    abstract member StoreSubscriptionRequest : JabberId -> PresenceStanza -> System.Threading.Tasks.Task
    abstract member RetrieveStoredSubscriptionRequests : unit -> Task<(JabberId * PresenceStanza) list>
    abstract member RemoveSubscriptionRequest : JabberId -> System.Threading.Tasks.Task
    abstract member GetCurrentPresence : unit -> Task<PresenceStatus>
    abstract member SetCurrentPresence : PresenceStatus -> System.Threading.Tasks.Task


type IRosterStore = 
    /// Returns the roster for the given user, creates one if necessary (ignores resource part)
    abstract member GetRoster : JabberId -> IUserRoster Task
    /// Checks if the given user exists
    abstract member ExistsUser : JabberId -> bool

type ConcurrentUserRoster (lock:AsyncLock, prim : IUserRoster) =
    let lockTask t =
        lock.Lock (fun () -> t |> Lazy.force |> Task.await) |> Async.StartAsTaskImmediate
    let lockTaskPlain t =
        lock.Lock (fun () -> t |> Lazy.force |> Task.awaitPlain) |> Async.StartAsTaskImmediate :> System.Threading.Tasks.Task

    interface IUserRoster with
        member __.GetCurrentRosterVersion () = lockTask <| lazy prim.GetCurrentRosterVersion()
        member __.GetRosterSize () = lockTask <| lazy prim.GetRosterSize()
        member __.UpdateOrAddRosterItem b item = lockTask <| lazy prim.UpdateOrAddRosterItem b item
        member __.GetItems () = lockTask <| lazy prim.GetItems()
        member __.GetItem jid = lockTask <| lazy prim.GetItem jid
        member __.DeleteItem jid = lockTask <| lazy prim.DeleteItem jid
        member __.ChangesSince ver = lockTask <| lazy prim.ChangesSince ver
        member __.StoreSubscriptionRequest jid stanza = lockTaskPlain <| lazy prim.StoreSubscriptionRequest jid stanza
        member __.RetrieveStoredSubscriptionRequests  () = lockTask <| lazy prim.RetrieveStoredSubscriptionRequests()
        member __.RemoveSubscriptionRequest jid = lockTaskPlain <| lazy prim.RemoveSubscriptionRequest jid
        member __.GetCurrentPresence () = lockTask <| lazy prim.GetCurrentPresence()
        member __.SetCurrentPresence v = lockTaskPlain <| lazy prim.SetCurrentPresence v

type ConcurrentRosterStore (prim : IRosterStore) =
    let locks = new System.Collections.Concurrent.ConcurrentDictionary<_,_>()
    interface IRosterStore with
        member __.GetRoster jid = 
            async {
                let! roster = prim.GetRoster jid |> Task.await
                return ConcurrentUserRoster(locks.GetOrAdd(jid.BareId, new AsyncLock()), roster) :> IUserRoster
            } |> Async.StartAsTaskImmediate
        member __.ExistsUser jid = prim.ExistsUser jid

/// simple memory store
type MemoryUserRoster (parent: IRosterStore, user:JabberId) = 
    let mutable rosterVersion = 3
    let mutable status = PresenceStatus.SetStatusUnavailable []
    let presenceRequest = new System.Collections.Generic.Dictionary<_,_>()
    let roster = new System.Collections.Generic.Dictionary<_,_>()
    //let changes = new System.Collections.Generic.Dictionary<_,_>()
    let AsRosterVersion () = rosterVersion.ToString()
    let NextRosterVersion () = 
        rosterVersion <- rosterVersion + 1
        AsRosterVersion () 
    member __.User  = user
    member __.ParentStore  = parent
    interface IUserRoster with
        member __.GetCurrentRosterVersion () = Task.returnM <| AsRosterVersion ()
        //member x.Status 
        //    with get() = status and set v = status <- v
        member __.GetRosterSize () = Task.returnM <|roster.Count
        member __.UpdateOrAddRosterItem _ item = 
            roster.[item.Jid.BareId] <- item
            Task.returnM <| NextRosterVersion ()
        member __.GetItems () = 
            roster.Values |> Seq.toList |> Task.returnM
        member __.GetItem jid =
            if roster.ContainsKey jid.BareId then
                Some roster.[jid.BareId] |> Task.returnM
            else None |> Task.returnM
            // -> RosterItem
        member __.DeleteItem jid =
            roster.Remove (jid.BareId) |> ignore
            NextRosterVersion () |> Task.returnM
            // -> RosterVersion
        member __.ChangesSince _ =
            [] |> Task.returnM
            // -> (RosterChange * RosterVersion) list
        member __.GetCurrentPresence () = status |> Task.returnM

        member __.SetCurrentPresence v = 
                if (obj.ReferenceEquals(v, null)) then
                    raise <| new System.ArgumentNullException("value")
                status <- v
                () |> Task.returnM :> System.Threading.Tasks.Task
        member __.StoreSubscriptionRequest jid stanza = 
            presenceRequest.[jid.BareId] <- stanza
            () |> Task.returnM :> System.Threading.Tasks.Task
        member __.RemoveSubscriptionRequest jid  = 
            presenceRequest.Remove jid.BareId |> ignore
            () |> Task.returnM :> System.Threading.Tasks.Task
        member __.RetrieveStoredSubscriptionRequests () =
            presenceRequest |> Seq.map (fun kv -> JabberId.Parse kv.Key, kv.Value) |> Seq.toList |> Task.returnM

type MemoryRosterStore () = 
    let rosters = new System.Collections.Generic.Dictionary<_,_>()
    interface IRosterStore with
        member x.GetRoster jid = 
            match rosters.TryGetValue jid.BareId with
            | true, r -> r |> Task.returnM
            | false, _ ->
                let r = MemoryUserRoster(x :> IRosterStore, jid) :> IUserRoster
                rosters.Add (jid.BareId, r)
                r |> Task.returnM
        member __.ExistsUser jid = rosters.ContainsKey(jid.BareId) 

        
type IRawMessageStanzaPlugin =
    inherit IReceivePipelineProvider<MessageStanza>

type IMessagePipelineService =
    abstract MessagePipeline : IPluginManager<IRawMessageStanzaPlugin>

type MessagePipelinePlugin(registrar : IPluginManagerRegistrar) =
    
    let messagePluginManager = registrar.CreateManagerFor<IRawMessageStanzaPlugin>()
    
    let handleMessage (msgStanza:MessageStanza) = 
        async {
            // Run pipeline
            let! pipeResult =
                Pipeline.processManager "MessagePipline" (messagePluginManager) (msgStanza)
            let stanza = pipeResult.ResultElem
            if not pipeResult.IsHandled then 
                // It was not even routed ...
                Log.Err(fun () -> L "Received completely unprocessed Message Stanza: %A" stanza)
            do! pipeResult.ProcessTask |> Task.await
        }
        
    let genericHandle stanza name isContent parser serverHandler = 
        async {
        if isContent stanza then
            Log.Info (fun () -> L "handling %s element (IMessagePipelinePlugin)" name)
            let msgStanza = Stanza<_>.Create(stanza, parser stanza)
            return! serverHandler msgStanza
        } |> Log.TraceMe

    let messageStanzaReceivedPlugin =
        { new IRawStanzaPlugin with
            member __.ReceivePipeline = 
                { Pipeline.empty "IMessagePipelinePlugin Stanza Message Pipeline"  with
                    HandlerState = 
                        fun info -> 
                            let elem = info.Result.Element
                            if Parsing.isContentMessage elem then
                                HandlerState.ExecuteAndHandle
                            else HandlerState.Unhandled
                    Process =
                        fun info -> Async.StartAsTaskImmediate(genericHandle (info.Result.Element) "message" Parsing.isContentMessage Parsing.parseContentMessage handleMessage)
                } :> IPipeline<_>
        }

    do
        registrar.RegisterFor<IRawStanzaPlugin> messageStanzaReceivedPlugin
        
    interface IMessagePipelineService with
        member __.MessagePipeline = messagePluginManager
        
    interface IXmppPlugin with
        member __.Name = "IMessagePipelinePlugin"
        member x.PluginService = Service.FromInstance<IMessagePipelineService, _> x
