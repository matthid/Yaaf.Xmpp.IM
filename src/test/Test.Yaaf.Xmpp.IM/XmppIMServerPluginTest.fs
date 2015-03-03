// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
open FsUnit
open NUnit.Framework
open Yaaf.Helper
open System.Collections.Generic
open Yaaf.TestHelper
open Test.Yaaf.Xmpp
open Yaaf.Xmpp
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Runtime.Features
open Yaaf.Xmpp.IM.Server
open Yaaf.Xmpp.IM
open Swensen.Unquote
open Foq
open Yaaf.Xmpp.XmlStanzas

type ClientStatus =
    {
        IsInterested : bool
        IsAvailable : PresenceStanza option
    } with
    static member Interested = { IsInterested = true; IsAvailable = None }
    static member Offline = { IsInterested = false; IsAvailable = None }
    static member Online = { IsInterested = true; IsAvailable = Some (Parsing.createPresenceElement None None None (PresenceProcessingType.StatusInfo (PresenceStatus.SetStatus PresenceData.Empty)))}
    static member Available = { IsInterested = false; IsAvailable = Some (Parsing.createPresenceElement None None None (PresenceProcessingType.StatusInfo (PresenceStatus.SetStatus PresenceData.Empty)))}
    static member Create (isInterested, priority) = 
        { 
            IsInterested = isInterested; 
            IsAvailable = 
                Some (
                    Parsing.createPresenceElement 
                        None None None 
                        (PresenceProcessingType.StatusInfo 
                            (PresenceStatus.SetStatus { PresenceData.Empty with Priority = priority })))}

/// Helper class to setup all the mocks for the IMServerPlugin given the connection status and the setup rosters
/// This enables us to write 'nice' unit tests (see below)
type TestIMServerPluginEnvironment (clientConnections : IDictionary<_, _>, rosters : IDictionary<_,_>) =
    let domain = JabberId.Parse "yaaf.de"
    let remoteId = JabberId.Parse "test@yaaf.de/res_0"

    let connections =
        clientConnections.Keys
        |> Seq.map (fun barejid -> 
            let num = clientConnections.[barejid] |> List.toArray
            let connections =
                Array.init num.Length (fun i ->
                    let jid = JabberId.Parse (sprintf "%s/res_%d" barejid i)
                    let status = num.[i]
                    let imServerService =
                        Mock<IImServerService>()
                            .Setup(fun s -> <@ s.CurrentPresence @>).Returns(status.IsAvailable)
                            .Setup(fun s -> <@ s.InterestedResource @>).Returns(status.IsInterested)
                            .Create()
                    let xmlStanzaService =
                        Mock<IXmlStanzaService>()
                            .Setup(fun s -> <@ s.GenerateNextId() @>).Returns("GeneratedId")
                            .Create()
                    let xmppClient =
                        Mock<IXmppClient>()
                            .Setup(fun c -> <@ c.GetService<IImServerService>() @>).Returns(imServerService)
                            .Setup(fun c -> <@ c.LocalJid @>).Returns(domain)
                            .Setup(fun c -> <@ c.RemoteJid @>).Returns(jid)
                            .Setup(fun c -> <@ c.GetService<IXmlStanzaService>() @>).Returns(xmlStanzaService)
                            .Create()
                    xmppClient)
            barejid, connections)
        |> dict
    let rosterStore =
        rosters.Keys
        |> Seq.map (fun jid -> 
            let rosterItems = rosters.[jid]
            //let presence = connections.[jid]
            let rosterStore = 
                Mock<IUserRoster>()
                    .Setup(fun u -> <@ u.GetItems() @>).Returns(Task.FromResult rosterItems)
                    .Setup(fun u -> <@ u.GetItem(any()) @>).Calls<JabberId>(fun jid -> 
                        rosterItems |> Seq.tryFind(fun i -> i.Jid.BareId = jid.BareId) |> Task.FromResult)
                    .Setup(fun u -> <@ u.SetCurrentPresence(any()) @>).Returns(Task.FromResult ())
                    .Setup(fun u -> <@ u.StoreSubscriptionRequest (any()) (any()) @>).Returns(Task.FromResult ())
                    .Setup(fun u -> <@ u.GetCurrentRosterVersion () @>).Returns(Task.FromResult "RosterVersion")
                    .Setup(fun u -> <@ u.UpdateOrAddRosterItem (any()) (any()) @>).Returns(Task.FromResult "RosterVersion")
                    .Setup(fun u -> <@ u.DeleteItem (any()) @>).Returns(Task.FromResult "RosterVersion")
                    .Setup(fun u -> <@ u.ChangesSince (any()) @>).Returns(Task.FromResult [])
                    .Setup(fun u -> <@ u.GetRosterSize () @>).Returns(Task.FromResult (List.length rosterItems))
                    .Setup(fun u -> <@ u.GetCurrentPresence() @>).Returns(Task.FromResult (PresenceStatus.SetStatus PresenceData.Empty))
                    .Create()
            jid, rosterStore)
        |> dict
    
    let conMgr = 
        Mock<IConnectionManager>()
            .Setup(fun c -> <@ c.GetConnections(any()) @>).Calls<JabberId>(fun jid -> 
                let items = connections.[jid.BareId]
                if jid.Resource.IsSome then
                    [ items.[System.Int32.Parse (jid.Resource.Value.Substring(4)) ] ]
                else
                    items |> Array.toList)
            .Create()
    let delivery =
        Mock<IStanzaDelivery>()
            .Setup(fun d -> <@ d.TryDeliver (any()) (any()) @>).Calls<JabberId * Stanza>(fun (jid, raw) -> 
                if jid.IsSpecialOf domain then
                    let con = connections.[jid.BareId].[System.Int32.Parse (jid.Resource.Value.Substring 4)]
                    let stanzas = con.GetService<IXmlStanzaService>()
                    stanzas.QueueStanza None raw
                async.Return true)
            .Create()
    let serverApi = 
        Mock<IServerApi>()
            .Setup(fun s -> <@ s.IsLocalJid(any()) @>).Returns(true)
            .Setup(fun s -> <@ s.ConnectionManager @>).Returns(conMgr)
            .Setup(fun s -> <@ s.Delivery @>).Returns(delivery)
            .Create()
    let negotationService = 
        Mock<INegotiationService>()
            .Setup(fun n -> <@ n.RemoteJid @>).Returns(remoteId)
            .Create()
    let shutdownTask = new System.Threading.Tasks.TaskCompletionSource<exn option>()
    
    let shutdown = 
        Mock<IRuntimeShutdown>()
            .Setup(fun s -> <@ s.RuntimeTask @>).Returns(shutdownTask.Task)
            .Create()
    let runtimeConfig = 
        Mock<IRuntimeConfig>()
            .Setup(fun c -> <@ c.StreamType @>).Returns(StreamType.ClientStream)
            .Create()
    let featureManager = 
        Mock<IServicePluginManager<IStreamFeatureHandler>>()
            .Setup(fun s -> <@ s.HasPluginOf<SessionFeature>() @>).Returns(true)
            .Setup(fun s -> <@ s.HasPluginOf<RosterVerFeature>() @>).Returns(true)
            .Create()
    let streamFeatureMgr = 
        Mock<IStreamFeatureService>()
            .Setup(fun s -> <@ s.FeatureManager @>).Returns(featureManager)
            .Create()
    let pluginMgr = 
        Mock<IXmppPluginManager>()
            .Setup(fun m -> <@ m.GetPluginService<IStreamFeatureService>() @>).Returns(streamFeatureMgr)
            .Setup(fun m -> <@ m.GetPluginService<IXmppClient>() @>).Returns(connections.["test@yaaf.de"].[0])
            .Create()

    let rosterStore =
        Mock<IRosterStore>()
            .Setup(fun r -> <@ r.ExistsUser(any()) @>).Calls<JabberId>(fun jid -> jid.IsSpecialOf domain)
            .Setup(fun r -> <@ r.GetRoster(any()) @>).Calls<JabberId>(fun jid -> Task.FromResult (rosterStore.[jid.BareId]))
            .Create()
    let config = 
        Mock<IImServerConfig>()
            .Setup(fun c -> <@ c.RosterStore @>).Returns(rosterStore)
            .Setup(fun c -> <@ c.AutoApprove @>).Returns(fun a b -> false)
            .Setup(fun c -> <@ c.AsyncCalls @>).Returns(AsyncType.Sync)
            .Create()
    
    let stanzas = connections.["test@yaaf.de"].[0].GetService<IXmlStanzaService>()
    let registrar, pipeline =
        let t = new System.Threading.Tasks.TaskCompletionSource<IRawStanzaPlugin>() 
        Mock<IPluginManagerRegistrar>()
            .Setup(fun p -> <@ p.RegisterFor<IRawStanzaPlugin>(any()) @>).Calls<IRawStanzaPlugin>(fun arg -> t.SetResult arg)
            .Create(),
        t.Task
    let plugin = IMServerPlugin(serverApi, negotationService, runtimeConfig, pluginMgr, config, stanzas, registrar, shutdown)
    let pipeline = pipeline.Result
    
    member x.RemoteId = remoteId
    member x.Plugin = plugin
    member x.Pipeline = pipeline
    member x.RosterStores = rosterStore
    member x.Connections = connections
    member x.Shutdown () = ()

type ``Test-Yaaf-Xmpp-IM-Server-IMServerPlugin: check XMPP_IM specification``() =
    inherit MyTestClass()
           
    [<Test>]
    member x.``Check that initial presence works`` () = 
        let clientConnections = 
            [ ("test@yaaf.de", [ ClientStatus.Offline; ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ]); 
              ("contact@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ]);
              ("other@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ]) ] |> dict
        let rosterStore =
            [ ("test@yaaf.de", 
                [ { RosterItem.CreateEmpty (JabberId.Parse "contact@yaaf.de") with Subscription = Some Both } ])
              ("contact@yaaf.de", 
                [ { RosterItem.CreateEmpty (JabberId.Parse "test@yaaf.de") with Subscription = Some Both } ])
              ("other@yaaf.de", []) ]
            |> dict
        let env = new TestIMServerPluginEnvironment(clientConnections, rosterStore)

        // Client 0 (resource 0) goes online (SEE 4.2.2.)
        let elem = System.Xml.Linq.XElement.Parse (sprintf "<presence xmlns='%s' />" KnownStreamNamespaces.clientNS) 
        let stanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS elem
        let result = Pipeline.processPipeline "testPipeline" [env.Pipeline.ReceivePipeline] stanza |> Async.StartAsTask |> waitTask
        result.ProcessTask |> waitTask
        

        // online contacts should receive the presence
        for con in env.Connections.["contact@yaaf.de"] do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            if imService.AvailableResource then 
                let expectedElem = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<presence xmlns='%s' from='test@yaaf.de/res_0' to='%s' />" KnownStreamNamespaces.clientNS con.RemoteJid.BareId) 
                let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)
            else 
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)


        let con = env.Connections.["test@yaaf.de"].[0]
        let stanzaService = con.GetService<IXmlStanzaService>()

        // Receive initial presence back
        let expectedElem = System.Xml.Linq.XElement.Parse (sprintf "<presence xmlns='%s' from='test@yaaf.de/res_0' to='test@yaaf.de' />" KnownStreamNamespaces.clientNS) 
        let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
        Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
        
        // Other available resources should receive this as well (SEE 4.2.3.)
        for con in env.Connections.["test@yaaf.de"] |> Seq.skip 1 do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            if imService.AvailableResource then 
                let expectedElem = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<presence xmlns='%s' from='test@yaaf.de/res_0' to='test@yaaf.de' />" KnownStreamNamespaces.clientNS) 
                let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)
            else 
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)


        // Receive presence from other resources in this case res_1 and res_4 (SEE 4.3.2. no probes required in this case)
        let expectedElem = System.Xml.Linq.XElement.Parse (sprintf "<presence xmlns='%s' id='GeneratedId' from='test@yaaf.de/res_1' to='test@yaaf.de' />" KnownStreamNamespaces.clientNS) 
        let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
        Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
        let expectedElem = System.Xml.Linq.XElement.Parse (sprintf "<presence xmlns='%s' id='GeneratedId' from='test@yaaf.de/res_4' to='test@yaaf.de' />" KnownStreamNamespaces.clientNS) 
        let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
        Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)

        // Receive presence from contacts available resources
        let expectedElem = System.Xml.Linq.XElement.Parse (sprintf "<presence xmlns='%s' id='GeneratedId' from='contact@yaaf.de/res_0' to='test@yaaf.de' />" KnownStreamNamespaces.clientNS) 
        let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
        Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
        let expectedElem = System.Xml.Linq.XElement.Parse (sprintf "<presence xmlns='%s' id='GeneratedId' from='contact@yaaf.de/res_3' to='test@yaaf.de' />" KnownStreamNamespaces.clientNS) 
        let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
        Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
        
        // all notifications combined = 5
        Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.exactly 5)
            
        // other contacts should receive nothing
        for con in env.Connections.["other@yaaf.de"] do
            let stanzaService = con.GetService<IXmlStanzaService>()
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)

            
    [<Test>]
    member x.``Check that directed presence works`` () = 
        let clientConnections = 
            [ ("test@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline ]); 
              ("contact@yaaf.de", [ ClientStatus.Online ]);
              ("other@yaaf.de", [ ClientStatus.Online ]) ] |> dict
          
        let rosterStore =
            [ ("test@yaaf.de", 
                [ { RosterItem.CreateEmpty (JabberId.Parse "contact@yaaf.de") with Subscription = Some Both } ])
              ("contact@yaaf.de", 
                [ { RosterItem.CreateEmpty (JabberId.Parse "test@yaaf.de") with Subscription = Some Both } ])
              ("other@yaaf.de", []) ]
            |> dict

        let env = new TestIMServerPluginEnvironment(clientConnections, rosterStore)

        // Send directed presence (SEE 4.6)
        let elem = 
            System.Xml.Linq.XElement.Parse (
                sprintf "<presence xmlns='%s' from='%s' to='other@yaaf.de'/>" KnownStreamNamespaces.clientNS env.RemoteId.FullId)
        let stanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS elem
        let result = Pipeline.processPipeline "testPipeline" [env.Pipeline.ReceivePipeline] stanza |> Async.StartAsTask |> waitTask
        result.ProcessTask |> waitTask

        // Directed presence should be sent to all available resources of other@yaaf.de
        for con in env.Connections.["other@yaaf.de"] do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            if imService.AvailableResource then 
                let expectedElem = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<presence xmlns='%s' from='test@yaaf.de/res_0' to='other@yaaf.de' />" KnownStreamNamespaces.clientNS) 
                let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)
            else 
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)
        
        // Check that nothing was sent to contact
        for con in env.Connections.["contact@yaaf.de"] do
            let stanzaService = con.GetService<IXmlStanzaService>()
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)

        // Check that nothing was sent ourself
        for con in env.Connections.["test@yaaf.de"] do
            let stanzaService = con.GetService<IXmlStanzaService>()
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)
        ()

    [<Test>]
    member x.``Check Subscription Request (No Subscription)`` () = 
        let clientConnections = 
            [ ("test@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ])
              ("contact@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ])
              ("other@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ]) 
              ] |> dict
          
        let rosterStore =
            [ ("test@yaaf.de", [])
              ("contact@yaaf.de", [])
              ("other@yaaf.de", []) ]
            |> dict

        let env = new TestIMServerPluginEnvironment(clientConnections, rosterStore)

        // Send subscription request (3.1.2)
        let elem = 
            System.Xml.Linq.XElement.Parse (
                // NOTE: specification says we SHOULD ignore the resource part, our implementation does this
                sprintf "<presence xmlns='%s' id='testId' to='contact@yaaf.de/ignoreres' type='subscribe' />" KnownStreamNamespaces.clientNS)
        let stanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS elem
        let result = Pipeline.processPipeline "testPipeline" [env.Pipeline.ReceivePipeline] stanza |> Async.StartAsTask |> waitTask
        result.ProcessTask |> waitTask

        // roster push for all interested resources
        for con in env.Connections.["test@yaaf.de"] do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            if imService.InterestedResource then 
                let expectedElem = 
                    System.Xml.Linq.XElement.Parse (
                        sprintf "<iq xmlns='%s' id='GeneratedId' to='%s' type='set'>
      <query ver='RosterVersion' xmlns='jabber:iq:roster'>
        <item jid='contact@yaaf.de' ask='subscribe' subscription='none'/>
      </query>
</iq>" KnownStreamNamespaces.clientNS con.RemoteJid.FullId)
                let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)
            else 
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)

        
        // Inbound (3.1.3), send request to all available resources
        for con in env.Connections.["contact@yaaf.de"] do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            if imService.AvailableResource then 
                let expectedElem = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<presence xmlns='%s' id='testId' from='test@yaaf.de' to='contact@yaaf.de' type='subscribe' />" KnownStreamNamespaces.clientNS) 
                let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)
            else 
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)
        
        // Check that nothing was sent to other
        for con in env.Connections.["other@yaaf.de"] do
            let stanzaService = con.GetService<IXmlStanzaService>()
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)

        // Check if store is modified properly
        let userStore = (env.RosterStores.GetRoster (JabberId.Parse "test@yaaf.de")).Result
        let expectedItem = { RosterItem.CreateEmpty (JabberId.Parse "contact@yaaf.de") with Ask = Some AskType.Subscribe; Subscription = Some SubscriptionType.SubsNone}
        Mock.Verify (<@ userStore.UpdateOrAddRosterItem (any()) (expectedItem) @>, Times.Once)
        Mock.Verify (<@ userStore.UpdateOrAddRosterItem (any()) (any()) @>, Times.Once)
        
        let userStore = (env.RosterStores.GetRoster (JabberId.Parse "contact@yaaf.de")).Result
        Mock.Verify (<@ userStore.UpdateOrAddRosterItem (any()) (any()) @>, Times.Never)

        let userStore = (env.RosterStores.GetRoster (JabberId.Parse "other@yaaf.de")).Result
        Mock.Verify (<@ userStore.UpdateOrAddRosterItem (any()) (any()) @>, Times.Never)
        ()
        
    [<Test>]
    member x.``Check Subscription Request (With Subscription)`` () = 
        // Check that server is auto-responding, when subscription is is place
        Assert.Inconclusive "Not implemented" 
        
    [<Test>]
    member x.``Check Subscription Request (With AutoApprove)`` () = 
        // Check that server is auto-responding, when autoapprove function returns true
        Assert.Inconclusive "Not implemented" 

    [<Test>]
    member x.``Check that stored incomming Subscription Request is resent on initial Presence`` () = 
        // when we have a saved subscription request that we do resend it
        Assert.Inconclusive "Not implemented" 

    [<Test>]
    member x.``Check that pending outgoing Subscription Request is resent on initial Presence`` () = 
        // when we have items with Ask=subscribe flag we should resend the request (see 3.1.2.)
        Assert.Inconclusive "Not implemented" 

    [<Test>]
    member x.``Check that approving works (Ask flag set)`` () = 
        let clientConnections = 
            [ ("test@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ])
              ("contact@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ])
              ("other@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline; ClientStatus.Interested; ClientStatus.Available ]) 
              ] |> dict
          
        let rosterStore =
            [ ("test@yaaf.de", [])
              ("contact@yaaf.de", [{RosterItem.CreateEmpty (JabberId.Parse "test@yaaf.de") with Ask = Some AskType.Subscribe; Subscription = Some SubsNone }])
              ("other@yaaf.de", []) ]
            |> dict

        let env = new TestIMServerPluginEnvironment(clientConnections, rosterStore)

        // Approve the request
        let elem = 
            System.Xml.Linq.XElement.Parse (
                // NOTE: specification says we SHOULD ignore the resource part, our implementation does this
                sprintf "<presence xmlns='%s' id='testId' to='contact@yaaf.de/ignoreres' type='subscribed' />" KnownStreamNamespaces.clientNS)
        let stanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS elem
        let result = Pipeline.processPipeline "testPipeline" [env.Pipeline.ReceivePipeline] stanza |> Async.StartAsTask |> waitTask
        result.ProcessTask |> waitTask

        // Approve should be redirected to interested resources
        for con in env.Connections.["contact@yaaf.de"] do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            let mutable callCount = 0
            if imService.InterestedResource then 
                let expectedPresence = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<presence xmlns='%s' id='testId' from='test@yaaf.de' to='contact@yaaf.de' type='subscribed' />" KnownStreamNamespaces.clientNS) 
                let expectedPresenceStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedPresence
                let expectedPush = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<iq xmlns='%s' id='GeneratedId' to='%s' type='set'>
      <query ver='RosterVersion' xmlns='jabber:iq:roster'>
        <item jid='test@yaaf.de' subscription='to'/>
      </query>
    </iq>" KnownStreamNamespaces.clientNS con.RemoteJid.FullId) 
                let expectedPushStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedPush
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedPresenceStanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedPushStanza) @>, Times.Once)
                callCount <- callCount + 2
            if imService.AvailableResource then
                // TODO: ID should be mirrored from initial stanza (should already work, test it here)
                let expectedRes_0 = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<presence xmlns='%s' from='test@yaaf.de/res_0' to='contact@yaaf.de' />" KnownStreamNamespaces.clientNS) 
                let expectedRes_0Stanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedRes_0
                
                let expectedRes_3 = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<presence xmlns='%s' from='test@yaaf.de/res_3' to='contact@yaaf.de' />" KnownStreamNamespaces.clientNS) 
                let expectedRes_3Stanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedRes_3
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedRes_0Stanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedRes_3Stanza) @>, Times.Once)
                callCount <- callCount + 2
                //Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)
             
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.exactly callCount)
        
        // Roster Push
        for con in env.Connections.["test@yaaf.de"] do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            let mutable callCount = 0
            if imService.InterestedResource then 
                let expectedPush = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<iq xmlns='%s' id='GeneratedId' to='%s' type='set'>
      <query ver='RosterVersion' xmlns='jabber:iq:roster'>
        <item jid='contact@yaaf.de' subscription='from'/>
      </query>
    </iq>" KnownStreamNamespaces.clientNS con.RemoteJid.FullId) 
                let expectedPushStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedPush
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedPushStanza) @>, Times.Once)

        // TODO: Verify roster was set properly
    [<Test>]
    member x.``Check that approving works (request is ignored invalid state)`` () = 
        Assert.Inconclusive "Not implemented" 
(*
    [<Test>]
    member x.``Check that we can request the roster`` () = 
        let receiver =  NUnitHelper.StanzaReceiver x.XmppClients.[0]
        let result = receiver |> NUnitHelper.readNext |> Async.StartAsTask
        let boundJid_0 = x.XmppClients.[0].LocalJid
        x.XmppClients.[0].WriteRaw (sprintf "<iq from='%s'
       id='hu2bac18'
       type='get'>
     <query xmlns='jabber:iq:roster'/>
   </iq>" boundJid_0.FullId) |> x.WaitProcess
        let stanza, result  = NUnitHelper.NEXT result 
        Parsing.isContentRoster stanza |> should be True
        let rosterAction = Parsing.parseContentRoster stanza
        match rosterAction with
        | RosterAction.RosterData (items, version) ->
            ()
        | _ -> Assert.Fail "expected roster result: RosterAction.RosterData"
        ()

    [<Test>]
    member x.``Check that roster errors are correct`` () = 
        let receiver =  NUnitHelper.StanzaReceiver x.XmppClients.[0]
        let result = receiver |> NUnitHelper.readNext |> Async.StartAsTask
        let boundJid_0 = x.XmppClients.[0].LocalJid
        let boundJid_1 = x.XmppClients.[1].LocalJid
        
        x.XmppClients.[1].WriteRaw "<finishnegotiation/>" |> x.WaitProcess // force end of negotiation procedure
        x.ServerNegotiatedClient.[1] |> waitTaskI "client 1 connected to server"

        // Error: Roster set initiated by unauthorized entity 
        x.XmppClients.[0].WriteRaw (sprintf "<iq from='test0@nunit.org'
       id='ix7s53v2'
       to='%s'
       type='set'>
     <query xmlns='jabber:iq:roster'>
       <item jid='nurse@example.com'/>
     </query>
   </iq>" boundJid_1.FullId) |> x.WaitProcess
        let stanza, result  = NUnitHelper.NEXT result 
        stanza.Header.Type |> should be (equal (Some "error"))
        stanza.Header.Id |> should be (equal (Some "ix7s53v2"))
        stanza.Header.StanzaType |> should be (equal XmlStanzas.XmlStanzaType.Iq)
        //stanza.Header.From.IsNone |> should be True
        //stanza.Header.From.Value.BareId |> should be (equal "test0@nunit.org")
        stanza.Header.To.IsSome |> should be True
        stanza.Header.To.Value.FullId |> should be (equal boundJid_0.FullId)
        // Check if <error type=auth><forbidden  /></error>

        // Error: Roster set contains more than one item 
        x.XmppClients.[0].WriteRaw "<iq from='juliet@example.com/balcony'
       id='nw83vcj4'
       type='set'>
     <query xmlns='jabber:iq:roster'>
       <item jid='nurse@example.com'
             name='Nurse'>
         <group>Servants</group>
       </item>
       <item jid='mother@example.com'
             name='Mom'>
         <group>Family</group>
       </item>
     </query>
   </iq>" |> x.WaitProcess
        let stanza, result  = NUnitHelper.NEXT result 
        stanza.Header.Type |> should be (equal (Some "error"))
        stanza.Header.Id |> should be (equal (Some "nw83vcj4"))
        stanza.Header.StanzaType |> should be (equal XmlStanzas.XmlStanzaType.Iq)
        // Check if <error type=modify><bad-request /></error>

        // Error: Roster set contains duplicate groups 
        x.XmppClients.[0].WriteRaw "<iq from='juliet@example.com/balcony'
       id='tk3va749'
       type='set'>
     <query xmlns='jabber:iq:roster'>
       <item jid='nurse@example.com'
             name='Nurse'>
         <group>Servants</group>
         <group>Servants</group>
       </item>
     </query>
   </iq>" |> x.WaitProcess
        let stanza, result  = NUnitHelper.NEXT result 
        stanza.Header.Type |> should be (equal (Some "error"))
        stanza.Header.Id |> should be (equal (Some "tk3va749"))
        stanza.Header.StanzaType |> should be (equal XmlStanzas.XmlStanzaType.Iq)
        // Check if <error type=modify><bad-request /></error>

        // Error: Roster set contains empty group 
        x.XmppClients.[0].WriteRaw "<iq from='juliet@example.com/balcony'
       id='fl3b486u'
       type='set'>
     <query xmlns='jabber:iq:roster'>
       <item jid='nurse@example.com'
             name='Nurse'>
         <group></group>
       </item>
     </query>
   </iq>" |> x.WaitProcess
        let stanza, result  = NUnitHelper.NEXT result 
        stanza.Header.Type |> should be (equal (Some "error"))
        stanza.Header.Id |> should be (equal (Some "fl3b486u"))
        stanza.Header.StanzaType |> should be (equal XmlStanzas.XmlStanzaType.Iq)
        // Check if <error type=modify><not-acceptable /></error>
*)