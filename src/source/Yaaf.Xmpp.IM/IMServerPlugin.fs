// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.IM.Server

open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Runtime.Features
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.IM

open Yaaf.Logging
open System

type AsyncType =
    | Sync
    | AsTask
    | AsTaskImmediate with
    member x.StartAsTask a =
        match x with
        | AsTask -> Async.StartAsTask a
        | AsTaskImmediate -> Async.StartAsTaskImmediate a
        | Sync -> Async.RunSynchronously a |> Task.FromResult
   
type IImServerConfig =
    abstract RosterStore : IRosterStore
    abstract SmallRoster : int
    abstract AsyncCalls : AsyncType
    abstract AutoApprove : (JabberId -> JabberId -> bool)
type ImServerConfig =
    {
        RosterStore : IRosterStore
        SmallRoster : int
        AsyncCalls : AsyncType
        AutoApprove : (JabberId -> JabberId -> bool)
    } with
    interface IImServerConfig with
        member x.RosterStore = x.RosterStore
        member x.SmallRoster = x.SmallRoster
        member x.AsyncCalls = x.AsyncCalls
        member x.AutoApprove = x.AutoApprove
    static member OfInterface (x:IImServerConfig) =
        {
            RosterStore = x.RosterStore
            SmallRoster = x.SmallRoster
            AsyncCalls = x.AsyncCalls
            AutoApprove = x.AutoApprove
        }
    static member Default =
        {
            RosterStore = Unchecked.defaultof<_>
            SmallRoster = 20
            AsyncCalls = AsTaskImmediate
            AutoApprove = (fun (_:JabberId) (_:JabberId) -> false)
        }

/// <summary> The IImServerService represents the service provided by the <see cref='IMServerPlugin'/> plugin. </summary>
type IImServerService =
    /// True when the current resource is an 'interested' resource (see XMPP-IM for details)
    abstract InterestedResource : bool with get
    /// None when the resource is not available, and the current presence stanza when the resource is available (see XMPP-IM for details)
    abstract CurrentPresence : PresenceStanza option with get
    /// Notification when the status changes, the boolean indicates whether this is an initial presence.
    abstract StatusChanged : IObservable<PresenceStanza option * bool>

[<AutoOpen>]
module ImServerServiceExtensions =
    type IImServerService with
        member x.AvailableResource = x.CurrentPresence.IsSome
            
        member x.CurrentPresenceData =
            match x.CurrentPresence with
            | Some p ->
                match p.Data with
                | StatusInfo s ->
                    match s with
                    | PresenceStatus.SetStatus d -> Some d
                    | _ -> None
                | _ -> None
            | _ -> None
        member x.ForcePresenceData () =
            match x.CurrentPresenceData with
            | Some s -> s
            | None -> failwith "presence data is not available"

type internal PresenceContext =
    { Store : IRosterStore
      User : JabberId
      Stanza : PresenceStanza
      MaybeTarget : Lazy<JabberId>
      MaybeUserStore : Task<IUserRoster>
      MaybeTargetStore : Task<IUserRoster> }
    //member x.User with get() = x.RemoteJid
    member x.Target with get() = x.MaybeTarget.Value
    member x.UserStore with get() = x.MaybeUserStore.Result
    //member x.Store with get() = x.Context.RosterStore
    member x.TargetStore with get() = x.MaybeTargetStore.Result
    //member x.StampedStanza 
    //    with get() = { x.Stanza with Header = { x.Stanza.Header with From = Some x.User } }
    member x.BareStampedStanza 
        with get() = 
            x.Stanza.WithHeader 
                { x.Stanza.Header with 
                    From = Some x.User.BareJid
                    To = if x.Stanza.Header.To.IsSome then Some x.Stanza.Header.To.Value.BareJid else None } 

    member x.WithTarget jid = 
        let target = jid
        //let LtargetStore = lazy 
        { x with MaybeTarget = lazy target; MaybeTargetStore = (x.Store.GetRoster target) }
        
// NOTE: It is difficult to split this file further as the roster methods are using some presence methods.
type IMServerPlugin 
    (serverApi : IServerApi, neg : INegotiationService, runtimeConfig : IRuntimeConfig, mgr : IXmppPluginManager,
     config : IImServerConfig, stanzas : IXmlStanzaService, registrar : IPluginManagerRegistrar,
     shutdown : IRuntimeShutdown) =
    
    let mutable interestedResource =  false
    let mutable currentPresence = None
    let isAvailableResource () = currentPresence.IsSome
    let presenceChanged = new ObservableSource<_>()

    // allowed for directed presence
    let mutable directedPresenceWhitelist = []

    do
        // No server api so it will fail anyway, if we already have all apis we need, why fail?
        //if not (runtimeConfig.IsServerSide) then
        //    Configuration.configFail "This plugin is server only."
        if runtimeConfig.StreamType.OnClientStream then
            let streamFeatures = mgr.GetPluginService<IStreamFeatureService>()
            if not <| streamFeatures.FeatureManager.HasPluginOf<SessionFeature>() then
                Configuration.configFail "IMServerPlugin (on client stream) requires SessionFeature to work!"
            if not <| streamFeatures.FeatureManager.HasPluginOf<RosterVerFeature>() then
                Configuration.configFail "IMServerPlugin (on client stream) requires RosterVerFeature to work!"
        //if not <| mgr.HasPluginOf<IMServerMessagePlugin>() then
        //    Log.Warn (fun _ -> "In a normal use case you want to add the IMServerMessagePlugin as well!")
            
        if obj.ReferenceEquals(null, config.RosterStore) then
            Configuration.configFail "IMServerPlugin requires a RosterStore instance to work!"
        //if mgr.HasPluginOf<XmppServerDeliveryPlugin>() then
        //    Configuration.configFail "IMServerPlugin requires that the XmppServerPlugin is not active!"
        //if mgr.HasPluginOf<ClientPlugin.UnknownIqResponderPlugin>() then
        //    Configuration.configFail "IMServerPlugin requires that the XmppClientPlugin is not active!"


    let getPresenceContext (presence:PresenceStanza) = 
        let Ltarget = lazy presence.Header.To.Value
        let store = config.RosterStore
        let source = if runtimeConfig.StreamType.OnClientStream || presence.Header.From.IsNone then neg.RemoteJid else presence.Header.From.Value
        let LtargetStore = 
            match presence.Header.To with
            | Some s -> (store.GetRoster s)
            | None ->
                let source = System.Threading.Tasks.TaskCompletionSource<_>()
                source.SetException (System.InvalidOperationException("The presence has no To header, so MaybeUserStore is not available"))
                source.Task
        let LuserStore = store.GetRoster source

        { User = source
          Store = store
          Stanza = presence
          MaybeTarget = Ltarget
          MaybeUserStore = LuserStore
          MaybeTargetStore = LtargetStore
        } 

    
    let handleInBoundStatusInfo (c:PresenceContext) = 
        async {
            let _ =
                match c.Stanza.Data with
                | PresenceProcessingType.StatusInfo n -> n
                | _ -> 
                    assert false
                    failwith "invalid stanza in handle*StatusInfo"
            assert (not runtimeConfig.StreamType.OnClientStream || c.Stanza.Header.To.IsSome && serverApi.IsLocalJid c.Target)
        

            // 4.2.3.  Server Processing of Inbound Initial Presence
            // Upon receiving presence from the user, the contact's server MUST deliver the user's presence stanza to all of the contact's available resources. 
            //let newResource = msgStanza.Header.From.Value
            do! serverApi.OnAllSimple c.Target.BareJid
                    (fun client ->
                    async {
                        let imService = client.GetService<IImServerService>()
                        let stanzaService = client.GetService<IXmlStanzaService>()
                        if imService.AvailableResource then
                            let msgStanza =
                                c.Stanza.WithHeader  
                                    { c.Stanza.Header with 
                                        //From = Some newResource
                                        To = Some c.Target.BareJid } 
                            stanzaService.QueueStanzaGeneric None msgStanza
                    })
        }

    let handleOutBoundStatusInfo (c:PresenceContext) = 
        async {
            let newStatus =
                match c.Stanza.Data with
                | PresenceProcessingType.StatusInfo n -> n
                | _ -> 
                    assert false
                    failwith "invalid stanza in handle*StatusInfo"
            assert runtimeConfig.StreamType.OnClientStream
            assert (serverApi.IsLocalJid c.User)
        
            let isOnline, _ =  
                match newStatus with
                | SetStatus data -> true, data
                | SetStatusUnavailable status -> false, { PresenceData.Empty with Status = status }
            let isDirected = c.Stanza.Header.To.IsSome
            let isInitial = isOnline && not (isAvailableResource()) (*c.Context.AvailableResource*) && not isDirected
        
            //if isInitial then // now we say this resource is available
            
            let curResource = neg.RemoteJid
            if not isDirected then
                // presenceChanged event at the end!
                currentPresence <- Some c.Stanza

                // TODO: check if offline, and only save when this was the last resource
                do! c.UserStore.SetCurrentPresence newStatus |> Task.awaitPlain

                // outbound
                // 4.2.2.  Server Processing of Outbound Initial Presence
                // 4.4.2.  Server Processing of Subsequent Outbound Presence (the same?)
                // Upon receiving initial presence from a client, 
                // the user's server MUST send the initial presence stanza from the full JID <user@domainpart/resourcepart> 
                // of the user to all contacts that are subscribed to the user's presence; 
                // such contacts are those for which a JID is present in the user's roster with the 'subscription' attribute 
                // set to a value of "from" or "both".
                let! rosterList = c.UserStore.GetItems () |> Task.await
                rosterList 
                |> Seq.filter (fun roster -> roster.HasFrom) 
                |> Seq.map (fun item -> 
                    async {
                        let presenceStanza =
                            c.Stanza.WithHeader 
                                { c.Stanza.Header with 
                                    From = Some curResource
                                    To = Some item.Jid }
                        if serverApi.IsLocalJid item.Jid then
                            do! handleInBoundStatusInfo ({ c with Stanza = presenceStanza }.WithTarget item.Jid) |> Async.Ignore
                        else
                            let! _ = serverApi.Delivery.TryDeliver item.Jid presenceStanza.SimpleStanza
                            ()
                        ()
                    })
                |> Seq.map (config.AsyncCalls.StartAsTask)
                |> Seq.iter ignore
                //|> Seq.iter serverApi.TaskManager.TrackTask
             
                // The user's server MUST also broadcast initial presence from the user's newly available resource 
                // to all of the user's available resources, including the resource that generated the presence notification in the first place 
                // (i.e., an entity is implicitly subscribed to its own presence). 
                // Note that the rfcs don't require that the first result of a presence is the current resource itself
                // However this simplifies writing unit tests
            
                let msgStanza =
                    c.Stanza.WithHeader
                        { c.Stanza.Header with 
                            From = Some curResource
                            To = Some c.User.BareJid }
                if not <| shutdown.RuntimeTask.IsCompleted then
                    // send only to ourself if runtime is still up.
                    stanzas.QueueStanzaGeneric None msgStanza

                do! serverApi.OnAllSimpleWithoutSelf (mgr.GetPluginService<IXmppClient>()) c.User.BareJid
                        (fun client ->
                        async {
                            let imService = client.GetService<IImServerService>()
                            let stanzaService = client.GetService<IXmlStanzaService>()
                            if imService.AvailableResource then
                                // ignore result
                                stanzaService.QueueStanzaGeneric None msgStanza
                    })
                if not isOnline then
                    currentPresence <- None
                presenceChanged.Next (currentPresence, isInitial)
                
                // In the absence of presence information about the user's contacts, the user's server 
                // MUST also send presence probes to the user's contacts on behalf of the user as specified under Section 4.3. 
  
                // But we do have presence info for all internal contacts
                if isInitial then
                    rosterList 
                    |> Seq.filter (fun roster -> roster.HasTo) 
                    |> Seq.map (fun item -> item.Jid) 
                    // add ourselfs to receive presence of our other resources
                    |> Seq.append [neg.RemoteJid]
                    |> Seq.map (fun item -> 
                        async {
                            if serverApi.IsLocalJid item then
                                // Internal, we don't need probes
                                // send available presence from all available resources
                                let self = mgr.GetPluginService<IXmppClient>()
                                do! serverApi.OnAllSimple item (fun innerClient ->
                                        async {
                                            let imService = innerClient.GetService<IImServerService>()
                                        
                                            // ignore when the contexts are equals, because this case is already handled above
                                            if imService.AvailableResource && innerClient <> self then
                                                let presence = imService.CurrentPresence.Value
                                                let presenceElem = 
                                                    Parsing.createPresenceElement 
                                                        (Some <| stanzas.GenerateNextId()) 
                                                        (Some innerClient.RemoteJid)
                                                        (Some c.User.BareJid)
                                                        presence.Data
                                                stanzas.QueueStanzaGeneric None presenceElem
                                        })
                            else
                                // There is no point in sending probes we already have the info
                                // TODO: Repeat probe when there is no answer within a reasonable time
                                // TODO: Cache presence info from other servers.
                                let probe = 
                                    Parsing.createPresenceElement
                                        (Some <| stanzas.GenerateNextId())
                                        (Some c.User.BareJid)
                                        (Some item)
                                        PresenceProcessingType.PresenceProbe
                                let! _ = serverApi.Delivery.TryDeliver item probe.SimpleStanza
                                ()
                        })
                    |> Seq.map (config.AsyncCalls.StartAsTask)
                    |> Seq.iter ignore
                    //|> Seq.iter serverApi.TaskManager.TrackTask
            else
                // Directed presence
                let toJid = c.Stanza.Header.To.Value.BareJid
                let presenceStanza =
                    c.Stanza.WithHeader 
                        { c.Stanza.Header with 
                            From = Some curResource
                            To = Some toJid }
                if serverApi.IsLocalJid toJid then
                    do! handleInBoundStatusInfo ({ c with Stanza = presenceStanza }.WithTarget toJid) |> Async.Ignore
                else
                    let! _ = serverApi.Delivery.TryDeliver toJid presenceStanza.SimpleStanza
                    ()
                if isOnline then
                    directedPresenceWhitelist <- toJid :: directedPresenceWhitelist
                else
                    directedPresenceWhitelist <- directedPresenceWhitelist |> List.filter (fun w -> w.BareId <> toJid.BareId)
                ()
        }
    

    let handleInBoundSubscriptionRequest (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = SubscriptionRequest)
            assert (not runtimeConfig.StreamType.OnClientStream || c.Stanza.Header.To.IsSome && serverApi.IsLocalJid c.Target)
        
            // if automatically approved || (contact exists && user already has subscription) || contact already send "subscribed" before then
            let! item = c.TargetStore.GetItem c.User |> Task.await
            if config.AutoApprove c.User c.Target || 
                (c.Store.ExistsUser c.Target && item.IsSome &&
                    (item.Value.Subscription.IsSome && item.Value.Subscription.Value.HasFrom || 
                        item.Value.Approved.IsSome && item.Value.Approved.Value)) then
                // auto-reply with "subscribed"
                // TODO: Setup roster item if not done already
                Log.Verb (fun () -> "Auto reply with subscibed")
                let subscribed = PresenceProcessingType.SubscriptionApproval
                let stanza = 
                    Parsing.createPresenceElement 
                        (Some <| stanzas.GenerateNextId()) 
                        (Some c.Target.BareJid) (Some c.User.BareJid) subscribed
                let! _ = serverApi.Delivery.TryDeliver c.User stanza.SimpleStanza
                ()
            // else
            elif (c.Store.ExistsUser c.Target) then
                // if any contact resource available then
                    // send to all available resources the request
                // save the request in database
                do! serverApi.OnAllSimple c.Target.BareJid
                        (fun client ->
                          async {
                            let imService = client.GetService<IImServerService>()
                            let stanzaService = client.GetService<IXmlStanzaService>()
                            if imService.AvailableResource then
                                // deliver presence request
                                stanzaService.QueueStanzaGeneric None c.BareStampedStanza
                          })
                do! c.TargetStore.StoreSubscriptionRequest c.User c.Stanza |> Task.awaitPlain
                ()
            else // ignore completly
                ()
        }

    
    let handleOutBoundSubscriptionRequest (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = SubscriptionRequest)
            assert runtimeConfig.StreamType.OnClientStream
            assert (serverApi.IsLocalJid c.User)

            // 3.1.2.  Server Processing of Outbound Subscription Request
            let rawStanza = c.BareStampedStanza.SimpleStanza
            let bareTarget = c.Target.BareJid

            // TODO: handle exceptions and return error:
            // If the presence subscription request cannot be locally delivered or remotely routed (e.g., because the request is malformed, the local contact does not exist, the remote server does not exist, an attempt to contact the remote server times out, or any other error is determined or experienced by the user's server), then the user's server MUST return an appropriate error stanza to the user. An example follows. 
            if c.BareStampedStanza.Header.To.IsSome && serverApi.IsLocalJid bareTarget then
                Log.Verb (fun () -> "Delivering request locally")
                do! handleInBoundSubscriptionRequest c |> Async.Ignore
            else
                Log.Verb (fun () -> "Try to deliver subscription request")
                let! _ = serverApi.Delivery.TryDeliver bareTarget rawStanza
                ()

            Log.Verb (fun () -> "Save request in database")
            let! targetStore = c.UserStore.GetItem bareTarget |> Task.await
            let oldItem, onlyUpdate = 
                match targetStore with 
                | Some s -> s, true 
                | None -> RosterItem.CreateEmpty bareTarget, false
            
            let rosterItem, hasChanged = 
                if oldItem.Subscription.IsSome && oldItem.Subscription.Value.HasTo 
                then oldItem, false
                else
                    { oldItem with 
                        Ask = Some AskType.Subscribe
                        Subscription = 
                            if oldItem.Subscription.IsNone then
                                Some SubscriptionType.SubsNone
                            else oldItem.Subscription }, true
            let newVer = 
                if hasChanged then
                    c.UserStore.UpdateOrAddRosterItem onlyUpdate rosterItem
                else
                    c.UserStore.GetCurrentRosterVersion()
            let! newVer = newVer |> Task.await
            do! serverApi.OnAllSimple c.User.BareJid
                    (fun client ->
                      async {
                        let imService = client.GetService<IImServerService>()
                        let stanzaService = client.GetService<IXmlStanzaService>()
                        if imService.InterestedResource then
                            let stanza = Parsing.createRosterElement 
                                            (stanzaService.GenerateNextId()) 
                                            (*Some context.LocalJid*) None 
                                            (Some client.RemoteJid) 
                                            (RosterAction.RosterChange(RosterChange.SetItem rosterItem, Some newVer))
                            // ignore result
                            stanzaService.QueueStanzaGeneric None stanza
                      })
        }


    let handleInBoundSubscriptionApproval (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProcessingType.SubscriptionApproval)
            assert (not runtimeConfig.StreamType.OnClientStream || c.Stanza.Header.To.IsSome && serverApi.IsLocalJid c.Target)

            let! userStore = c.TargetStore.GetItem c.User |> Task.await
            let oldItem, onlyUpdate = 
                match userStore with Some s -> s, true | None -> RosterItem.CreateEmpty c.User, false
            // if contact in users roster with none, from && ask flag
            if oldItem.HasTo |> not && oldItem.HasAsk then
                let rosterItem = 
                    { oldItem with
                        Ask = None
                        Subscription = oldItem.Subscription |> SubscriptionType.OnOption (fun subs -> subs.AddTo()) |> Some }
                let! newVer = c.TargetStore.UpdateOrAddRosterItem onlyUpdate rosterItem |> Task.await
                // deliver presence stanza to all "interested" resources
                // Roster push to set subscription to "to" or "both"
                do! serverApi.OnAllSimple c.Target.BareJid
                        (fun client ->
                            async {
                                let imService = client.GetService<IImServerService>()
                                let stanzaService = client.GetService<IXmlStanzaService>()
                                if imService.InterestedResource then
                                    // deliver "subscribed" presence to interested resources
                                    stanzaService.QueueStanzaGeneric None c.BareStampedStanza
                                    
                                    let stanza = Parsing.createRosterElement 
                                                    (stanzaService.GenerateNextId()) 
                                                    (*Some context.LocalJid*) None 
                                                    (Some client.RemoteJid) 
                                                    (RosterAction.RosterChange(RosterChange.SetItem rosterItem, Some newVer))
                                    // roster push
                                    stanzaService.QueueStanzaGeneric None stanza
                            })
                // MAY: if no resource online save the SubscriptionApproval for later delivery

            // else ignore
        }

    let handleOutBoundSubscriptionApproval (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProcessingType.SubscriptionApproval)
            assert runtimeConfig.StreamType.OnClientStream
            assert (serverApi.IsLocalJid c.User)
        
            // outbound
            let msgStanza = c.BareStampedStanza
            // Stamp and deliver
            let rawStanza = msgStanza.SimpleStanza
            let bareTarget = c.Target.BareJid
            if (serverApi.IsLocalJid bareTarget) then
                do! handleInBoundSubscriptionApproval c |> Async.Ignore
            else
                let! _ = serverApi.Delivery.TryDeliver bareTarget rawStanza
                ()

            // Save in roster. Load roster and check if we have TO sbscription
            let! targetStoreItem = c.UserStore.GetItem bareTarget |> Task.await
            let oldItem, onlyUpdate = 
                match targetStoreItem with 
                | Some s -> s, true 
                | None -> RosterItem.CreateEmpty bareTarget, false
            let rosterItem = 
                { oldItem with
                    Subscription = oldItem.Subscription |> SubscriptionType.OnOption (fun subs -> subs.AddFrom()) |> Some }
            let! newVer = c.UserStore.UpdateOrAddRosterItem onlyUpdate rosterItem |> Task.await
            //let! currentPresence = c.UserStore.GetCurrentPresence() |> Task.await
            do! serverApi.OnAllSimple c.User.BareJid
                    (fun client ->
                    async {
                        let imService = client.GetService<IImServerService>()
                        let stanzaService = client.GetService<IXmlStanzaService>()
                        // RosterPush with To or From
                        if imService.InterestedResource then
                            let stanza = Parsing.createRosterElement 
                                            (stanzaService.GenerateNextId()) 
                                            (*Some context.LocalJid*) None 
                                            (Some client.RemoteJid) 
                                            (RosterAction.RosterChange(RosterChange.SetItem rosterItem, Some newVer))
                            stanzaService.QueueStanzaGeneric None stanza
                        // send our presence to the now allowed user
                        if imService.AvailableResource then
                            let toJid = c.Stanza.Header.To.Value.BareJid
                            let presenceStanza =
                                imService.CurrentPresence.Value.WithHeader 
                                    { imService.CurrentPresence.Value.Header with 
                                        From = Some client.RemoteJid
                                        To = Some toJid }
                            if serverApi.IsLocalJid toJid then
                                do! handleInBoundStatusInfo ({ c with Stanza = presenceStanza }.WithTarget toJid) |> Async.Ignore
                            else
                                let! _ = serverApi.Delivery.TryDeliver toJid presenceStanza.SimpleStanza
                                ()
                    })
        }

    
    let handleInBoundCancelSubscription (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProcessingType.DenySubscriptionRequestOrCancelSubscription)
            assert (not runtimeConfig.StreamType.OnClientStream || c.Stanza.Header.To.IsSome && serverApi.IsLocalJid c.Target)
        
            // inbound
            let! rosterItem = c.TargetStore.GetItem c.User |> Task.await
            // if contact in user roster with to or both then
            if c.Store.ExistsUser c.Target && rosterItem.IsSome && rosterItem.Value.HasTo then
                // deliver cancel presence to all interested resources
                // Roster push with None(was To or To+PendingIn) or From(was Both) 
                let rosterItem = rosterItem.Value
                let rosterItem = 
                    { rosterItem with
                        Subscription = rosterItem.Subscription |> SubscriptionType.OnOption (fun subs -> subs.RemoveTo()) |> Some
                        // TODO: double check
                        Approved = None }
                let! newVer = c.TargetStore.UpdateOrAddRosterItem true rosterItem |> Task.await
                do! serverApi.OnAllSimple c.Target.BareJid
                        (fun client ->
                        async {
                            let imService = client.GetService<IImServerService>()
                            let stanzaService = client.GetService<IXmlStanzaService>()
                            if imService.InterestedResource then
                                stanzaService.QueueStanzaGeneric None c.Stanza
                                let stanza = Parsing.createRosterElement 
                                                (stanzaService.GenerateNextId()) 
                                                (*Some context.LocalJid*) None 
                                                (Some client.RemoteJid) 
                                                (RosterAction.RosterChange(RosterChange.SetItem rosterItem, Some newVer))
                                // ignore result
                                stanzaService.QueueStanzaGeneric None stanza
                    })
                // MAY: Save stanza if offline, deliver when available resource is available

            //else ignore stanza
        }

    
    let handleOutBoundCancelSubscription (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProcessingType.DenySubscriptionRequestOrCancelSubscription)
            assert runtimeConfig.StreamType.OnClientStream
            assert (serverApi.IsLocalJid c.User)
        
            // outbound
            let! rosterItem = c.UserStore.GetItem c.Target |> Task.await
            // if target not in roster || target in roster with "None", "None + Pending out", "To" then ignore
            if rosterItem.IsNone || (not rosterItem.Value.HasFrom && not rosterItem.Value.HasApproved) then
                ignore ()
            // elif target in roster with approved flag then remove approved flag and ignore
            elif not rosterItem.Value.HasFrom && rosterItem.Value.HasApproved then
                let newItem = 
                    { rosterItem.Value with Approved = None }
                c.UserStore.UpdateOrAddRosterItem true newItem |> ignore
            // else
            else
                let rosterItem = rosterItem.Value
                // Send "unavailable" from all available resources to target
                do! serverApi.OnAllSimple c.User.BareJid
                        (fun client ->
                        async {
                            let imService = client.GetService<IImServerService>()
                            let stanzaService = client.GetService<IXmlStanzaService>()
                            // send our presence to the now not allowed target
                            if imService.AvailableResource then
                                let currentPresence = StatusInfo(SetStatusUnavailable [])
                                let stanza = Parsing.createPresenceElement (Some <| stanzaService.GenerateNextId()) (Some client.RemoteJid) (Some c.Target.BareJid) currentPresence
                                let! _ = serverApi.Delivery.TryDeliver c.Target stanza.SimpleStanza
                                ()
                        })
                // Send the bare id stamped presence stanza to the target
                let raw = c.BareStampedStanza.SimpleStanza
                let! res = serverApi.Delivery.TryDeliver c.Target raw
                if res then
                    if (serverApi.IsLocalJid c.Target) then
                        do! handleInBoundCancelSubscription c |> Async.Ignore
                    
                    let rosterItem = 
                        { rosterItem with
                            Subscription = rosterItem.Subscription |> SubscriptionType.OnOption (fun subs -> subs.RemoveFrom()) |> Some }
                    let! newVer = c.UserStore.UpdateOrAddRosterItem true rosterItem |> Task.await
                    
                    // Send roster push to all interested resource (now with None or To)
                    do! serverApi.OnAllSimple c.User.BareJid
                            (fun client ->
                            async {
                                let imService = client.GetService<IImServerService>()
                                let stanzaService = client.GetService<IXmlStanzaService>()
                                if imService.InterestedResource then
                                    let stanza = Parsing.createRosterElement 
                                                    (stanzaService.GenerateNextId()) 
                                                    (*Some context.LocalJid*) None 
                                                    (Some client.RemoteJid) 
                                                    (RosterAction.RosterChange(RosterChange.SetItem rosterItem, Some newVer))
                                    stanzaService.QueueStanzaGeneric None stanza
                            })
                ()
            ()
        }

    let handleInBoundUnsubscribe (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProcessingType.Unsubscribe)
            assert (not runtimeConfig.StreamType.OnClientStream || c.Stanza.Header.To.IsSome && serverApi.IsLocalJid c.Target)

            // inbound
            let! rosterItem = c.TargetStore.GetItem c.User |> Task.await
            // if user in contacts roster with from or both
            if c.Store.ExistsUser c.Target && rosterItem.IsSome && rosterItem.Value.HasFrom then
                // Update roster (now none or to)
                let rosterItem = rosterItem.Value
                let rosterItem = 
                    { rosterItem with
                        Subscription = rosterItem.Subscription |> SubscriptionType.OnOption (fun subs -> subs.RemoveFrom()) |> Some }
                let! newVer = c.TargetStore.UpdateOrAddRosterItem true rosterItem |> Task.await
                // deliver presence to contacts interested resources
                // send unavailable from all available resources to user
                do! serverApi.OnAllSimple c.Target.BareJid
                        (fun client ->
                        async {
                            let imService = client.GetService<IImServerService>()
                            let stanzaService = client.GetService<IXmlStanzaService>()
                            if imService.InterestedResource then
                                // deliver presence
                                stanzaService.QueueStanzaGeneric None c.Stanza
                                // Send roster push
                                let stanza = Parsing.createRosterElement 
                                                (stanzaService.GenerateNextId()) 
                                                (*Some context.LocalJid*) None 
                                                (Some client.RemoteJid) 
                                                (RosterAction.RosterChange(RosterChange.SetItem rosterItem, Some newVer))
                                // ignore result
                                stanzaService.QueueStanzaGeneric None stanza

                            if imService.AvailableResource then
                                // the contact is no longer interested
                                let currentPresence = StatusInfo (SetStatusUnavailable [])
                                let stanza = Parsing.createPresenceElement (Some <| stanzaService.GenerateNextId()) (Some client.RemoteJid) (Some c.User.BareJid) currentPresence
                                let! _ = serverApi.Delivery.TryDeliver c.User.BareJid stanza.SimpleStanza
                                ()
                        })
            else //ignore msgStanza
                // However, if the contact's server is keeping track of an inbound presence subscription request from the user to
                // the contact but the user is not yet in the contact's roster 
                // (functionally equivalent to a subscription state of "None + Pending In" where the contact never added the user 
                // to the contact's roster), then the contact's server MUST simply remove any record of the inbound presence 
                // subscription request (it cannot remove the user from the contact's roster because the user was never added to the contact's roster). 
                if c.Store.ExistsUser c.Target then
                    do! c.TargetStore.RemoveSubscriptionRequest c.User |> Task.awaitPlain
            ()
        }

    let handleOutBoundUnsubscribe (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProcessingType.Unsubscribe)
            assert runtimeConfig.StreamType.OnClientStream
            assert (serverApi.IsLocalJid c.User)
        
            // outbound
            // stamp stanza
            // deliver stanza 
            let raw = c.BareStampedStanza.SimpleStanza
            let! _ = serverApi.Delivery.TryDeliver c.Target.BareJid raw
            //(if local also do the else branch)
            if serverApi.IsLocalJid c.Target then
                do! handleInBoundUnsubscribe c |> Async.Ignore
            // Update roster (push) => now none or from
            let! maybeRosterItem = c.UserStore.GetItem c.Target |> Task.await
            match maybeRosterItem with
            | Some rosterItem ->
                let rosterItem = 
                    { rosterItem with
                        Subscription = rosterItem.Subscription |> SubscriptionType.OnOption (fun subs -> subs.RemoveTo()) |> Some }
                let! newVer = c.UserStore.UpdateOrAddRosterItem true rosterItem |> Task.await
                do! serverApi.OnAllSimple c.User.BareJid
                        (fun client ->
                        async {
                            let imService = client.GetService<IImServerService>()
                            let stanzaService = client.GetService<IXmlStanzaService>()
                            if imService.InterestedResource then
                                let stanza = Parsing.createRosterElement 
                                                (stanzaService.GenerateNextId()) 
                                                (*Some context.LocalJid*) None 
                                                (Some client.RemoteJid) 
                                                (RosterAction.RosterChange(RosterChange.SetItem rosterItem, Some newVer))
                                // ignore result
                                stanzaService.QueueStanzaGeneric None stanza
                    })
            | None -> ()
            ()
        }

    let handleInBoundPresenceProbe (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProbe)
            assert (not runtimeConfig.StreamType.OnClientStream || c.Stanza.Header.To.IsSome && serverApi.IsLocalJid c.Target)
        
            // 4.3.2.  Server Processing of Inbound Presence Probe

            // 1. If the contact account does not exist or the user's bare JID is in the contact's roster with a subscription state other than 
            // "From", "From + Pending Out", or "Both" (as explained under Appendix A), then the contact's server SHOULD return a presence stanza of type "unsubscribed" 
            // in response to the presence probe (this will trigger a protocol flow for canceling the user's subscription to the contact as described under Section 3.2;
            //  however, this MUST NOT result in cancellation of a subscription pre-approval as described under Section 3.4).
            let! item = c.TargetStore.GetItem c.User |> Task.await
            if not <| c.Store.ExistsUser c.Target || item.IsNone || item.Value.Subscription.IsNone || not item.Value.Subscription.Value.HasFrom then
                // However, if a server receives a presence probe from a configured domain of the server itself or another such trusted service, 
                // it MAY provide presence information about the user to that entity.
                let unsub = 
                    Parsing.createPresenceElement
                        (c.Stanza.Header.Id)
                        (Some c.Target)
                        (Some c.User)
                        PresenceProcessingType.DenySubscriptionRequestOrCancelSubscription
                let raw = unsub.SimpleStanza
                let! _ = serverApi.Delivery.TryDeliver c.User raw
                ()
            else
                // Send presence from all available resources
                let! tasks = 
                    serverApi.ConnectionManager.GetConnectionsSafeStart c.Target (fun innerClient ->
                        async {
                            let imService = innerClient.GetService<IImServerService>()
                            //let _ = innerClient.GetService<IXmlStanzaService>()
                            if imService.AvailableResource then
                                let stanza = imService.CurrentPresence.Value
                                let presenceElem = 
                                    stanza.WithHeader { stanza.Header with From = Some innerClient.RemoteJid; To = Some c.Target }
                                //let raw = Stanza.ToRawStanza Parsing.presenceContentGenerator (presenceElem)
                                //do! serverApi.Delivery.TryDeliver c.U
                                stanzas.QueueStanzaGeneric None presenceElem
                        })
                if tasks.Length = 0 then
                    // sadly no resource is available :( send an offline stanza instead
                    let! presence = c.TargetStore.GetCurrentPresence() |> Task.await
                    let stanza =
                        Parsing.createPresenceElement
                            (c.Stanza.Header.Id)
                            (Some c.Target)
                            (Some c.User)
                            (PresenceProcessingType.StatusInfo presence)
                    stanzas.QueueStanzaGeneric None stanza
                ()
        }

    let handleOutBoundPresenceProbe (c:PresenceContext) = 
        async {
            assert (c.Stanza.Data = PresenceProbe)
            assert runtimeConfig.StreamType.OnClientStream
            assert (serverApi.IsLocalJid c.User)
        
            // 4.3.1.  Server Generation of Outbound Presence Probe
            // When a server needs to discover the availability of a user's contact, it sends a presence probe from the bare JID <user@domainpart> of the user to the bare JID <contact@domainpart> of the contact. 
            //()
        }
    
    let handlePresence (presence:PresenceStanza) = 
        async {
            let c = getPresenceContext presence
            
            let isOutBound = runtimeConfig.StreamType.OnClientStream
            match presence.Data with
            | StatusInfo (_) -> 
                if isOutBound then
                    return! handleOutBoundStatusInfo c
                else 
                    return! handleInBoundStatusInfo c
            | SubscriptionRequest ->
                if isOutBound then
                    // 3.1.2.  Server Processing of Outbound Subscription Request
                    return! handleOutBoundSubscriptionRequest c
                else 
                    return! handleInBoundSubscriptionRequest c
            | SubscriptionApproval ->
                if isOutBound then
                    return! handleOutBoundSubscriptionApproval c
                else
                    return! handleInBoundSubscriptionApproval c
            | DenySubscriptionRequestOrCancelSubscription ->
                if isOutBound then
                    return! handleOutBoundCancelSubscription c
                else
                    return! handleInBoundCancelSubscription c
            | Unsubscribe ->
                if isOutBound then
                    return! handleOutBoundUnsubscribe c
                else
                    return! handleInBoundUnsubscribe c
            | PresenceProbe -> 
                // A "presence probe" is a request for a contact's current presence information, sent on behalf of a user by the user's server; 
                // syntactically it is a presence stanza whose 'type' attribute has a value of "probe". 
                // In the context of presence subscriptions, the value of the 'from' address MUST be the bare JID of the subscribed user 
                // and the value of the 'to' address MUST be the bare JID of the contact to which the user is subscribed, 
                // since presence subscriptions are based on the bare JID. 
                if isOutBound then
                    return! handleOutBoundPresenceProbe c
                else 
                    return! handleInBoundPresenceProbe c
        }

    let handleRoster (msgStanza:RosterStanza) = 
        async {
        let user = neg.RemoteJid
        let store = config.RosterStore
        let! userStore = store.GetRoster user |> Task.await
        match msgStanza.Data with 
        | RosterAction.GetRoster ver ->
            interestedResource <- true
            let! rosterSize = userStore.GetRosterSize() |> Task.await
            match ver with
            | Some v when rosterSize > config.SmallRoster -> // do pushes
                // send ack
                stanzas.QueueStanza None (Stanza.createEmptyIqResult (Some user) msgStanza.Header)
                
                // Push changes
                let! changes = userStore.ChangesSince v |> Task.await
                for change, version in changes do
                    let stanza = Parsing.createRosterElement (stanzas.GenerateNextId()) (None) (Some neg.RemoteJid) (RosterAction.RosterChange(change, Some version))
                    // ignore result
                    stanzas.QueueStanzaGeneric None stanza
            | _ -> // send complete roster
                let! items = userStore.GetItems() |> Task.await
                let! ver = userStore.GetCurrentRosterVersion() |> Task.await
                let stanza = Parsing.createRosterElement (msgStanza.Header.Id.Value) (None) (Some neg.RemoteJid) (RosterAction.RosterData(items, Some ver))
                // this is the result
                stanzas.QueueStanzaGeneric None stanza
        | RosterAction.RosterChange (change, _) ->
            let jid, isDelete = 
                match change with
                | RosterChange.DeleteItem jid -> jid, true
                | RosterChange.SetItem item -> item.Jid, false
            
            let! oldItem = userStore.GetItem jid |> Task.await
            if isDelete then
                // 2.5.2.  Success Case
                // Send required presence stanzas
                match oldItem with
                | Some item ->
                    match item.Subscription with
                    | Some subs -> 
                        if subs.HasTo then
                            // If the user has a presence subscription to the contact, then the user's server MUST send a presence stanza of type "unsubscribe" to the contact (in order to unsubscribe from the contact's presence). 
                            let presence = PresenceProcessingType.Unsubscribe
                            let stanza = Parsing.createPresenceElement (Some <| stanzas.GenerateNextId()) (Some user.BareJid) (Some jid.BareJid) presence
                            do! handlePresence stanza
                            ()
                        if subs.HasFrom then
                            // If the contact has a presence subscription to the user, then the user's server MUST send a presence stanza of type "unsubscribed" to the contact (in order to cancel the contact's subscription to the user). 
                            let presence = PresenceProcessingType.DenySubscriptionRequestOrCancelSubscription
                            let stanza = Parsing.createPresenceElement (Some <| stanzas.GenerateNextId()) (Some user.BareJid) (Some jid.BareJid) presence
                            do! handlePresence stanza
                    | None -> ()
                | None -> ()
            let! newVer =
                match change with
                | RosterChange.DeleteItem jid ->
                    // Delete item
                    userStore.DeleteItem jid
                | RosterChange.SetItem item ->
                    // We must ignore the suscription property:
                    let item = { item with Subscription = oldItem |> Option.bind (fun roster -> roster.Subscription) }
                    userStore.UpdateOrAddRosterItem true item 
                |> Task.await
            // send ack
            stanzas.QueueStanza None (Stanza.createEmptyIqResult (Some user) msgStanza.Header)

            // push change to all interested resources
            do! serverApi.OnAllSimple user
                 (fun client ->
                  async {
                   let imService = client.GetService<IImServerService>()
                   let stanzaService = client.GetService<IXmlStanzaService>()
                   if imService.InterestedResource then
                    let stanza = Parsing.createRosterElement (stanzaService.GenerateNextId()) (None) (Some client.RemoteJid) (RosterAction.RosterChange(change, Some newVer))
                    // ignore result
                    stanzas.QueueStanzaGeneric None stanza
                  })
        | RosterAction.RosterData _ ->
            return failwith "don't expect RosterData on this level!"
        }

    let genericHandle stanza name isContent parser serverHandler = 
        async {
        if isContent stanza then
            Log.Info (fun () -> L "handling %s element (ImServerPlugin)" name)
            let msgStanza = Stanza<_>.Create(stanza, (parser stanza))
            return! serverHandler msgStanza
        } |> Log.TraceMe
        
    let stanzaReceivedPlugin =
        { new IRawStanzaPlugin with
            member __.ReceivePipeline = 
                { Pipeline.empty "ImServerPlugin Stanza Pipeline"  with
                    HandlerState = 
                        fun info -> 
                            let elem = info.Result.Element
                            if Parsing.isContentPresence elem || (Parsing.isContentRoster elem && elem.Header.AddressedToOrNone neg.LocalJid) then
                                HandlerState.ExecuteAndHandle
                            else HandlerState.Unhandled
                    Process =
                        fun info -> 
                            async {
                                do! genericHandle (info.Result.Element) "presence" Parsing.isContentPresence Parsing.parseContentPresence handlePresence
                                do! genericHandle (info.Result.Element) "roster" Parsing.isContentRoster Parsing.parseContentRoster handleRoster
                            } |> Async.StartAsTaskImmediate
                } :> IPipeline<_>
        }
        
    do
        registrar.RegisterFor<IRawStanzaPlugin> stanzaReceivedPlugin
        
        shutdown.RuntimeTask.ContinueWith(fun (_:System.Threading.Tasks.Task<_>) ->
            if isAvailableResource() then
                // 4.5.2.  Server Processing of Outbound Unavailable Presence
                // We have to emulate an unavailable presence at this point
                try
                    let currentPresence = PresenceProcessingType.StatusInfo(PresenceStatus.SetStatusUnavailable StatusData.Empty)
                    let stanza = Parsing.createPresenceElement None None None currentPresence
                    handlePresence stanza |> Async.RunSynchronously
                    assert (not <| isAvailableResource())
                with exn ->
                    Log.Err (fun () -> L "Error while setting the already disconnected resource offline: %O" exn)
            )
            |> ignore

    interface IImServerService with
        member x.InterestedResource = interestedResource
        member x.CurrentPresence = currentPresence
        member x.StatusChanged = presenceChanged.AsObservable
        
    interface IXmppPlugin with
        member x.Name = "ImServerPlugin"
        member x.PluginService = Service.FromInstance<IImServerService, _> x


