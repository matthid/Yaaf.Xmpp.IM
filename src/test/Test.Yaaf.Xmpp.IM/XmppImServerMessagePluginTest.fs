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
open Yaaf.Xmpp.ServiceDiscovery
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Runtime.Features
open Yaaf.Xmpp.IM.Server
open Yaaf.Xmpp.IM
open Swensen.Unquote
open Foq
open Yaaf.Xmpp.XmlStanzas

/// Helper class to setup all the mocks for the IMServerPlugin given the connection status and the setup rosters
/// This enables us to write 'nice' unit tests (see below)
type TestIMServerMessagePluginEnvironment (clientConnections : IDictionary<_, _>, userConfig, offlineStorage : IOfflineMessageStorage option) =
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
            .Setup(fun s -> <@ s.IsLocalJid(any()) @>).Calls<JabberId>(fun jid -> jid.Domainpart = domain.Domainpart)
            .Setup(fun s -> <@ s.ConnectionManager @>).Returns(conMgr)
            .Setup(fun s -> <@ s.Delivery @>).Returns(delivery)
            .Create()
    let negotationService = 
        Mock<INegotiationService>()
            .Setup(fun n -> <@ n.RemoteJid @>).Returns(remoteId)
            .Setup(fun n -> <@ n.NegotiationCompleted @>).Returns(true)
            .Create()
    let runtimeConfig = 
        Mock<IRuntimeConfig>()
            .Setup(fun c -> <@ c.StreamType @>).Returns(StreamType.ClientStream)
            .Create()
    let messageUserConfig =
        Mock<IUserMessageUserConfig>()
            .Setup(fun m -> <@ m.GetConfig (any(), any()) @>).Calls<JabberId * JabberId option>(fun (_,_) ->
                Task.FromResult userConfig)
            .Create()
    let config = 
        Mock<IImServerMessagePluginConfig>()
            .Setup(fun c -> <@ c.OfflineStorage @>).Returns(offlineStorage)
            .Setup(fun c -> <@ c.UserConfig @>).Returns(messageUserConfig)
            .Create()
    
    let addressing = 
        Mock<IAddressingService>()
            .Setup(fun a -> <@ a.IsLocalStanzaOnServer(any(), (any())) @>).Calls<IServerApi * StanzaHeader>(fun (api, header) -> header.To.IsNone || (header.To.Value.Localpart.IsNone && api.IsLocalJid header.To.Value))
            .Create()
    let stanzas = connections.["test@yaaf.de"].[0].GetService<IXmlStanzaService>()
    let createPluginManager () = 
        Mock<IPluginManager<_>>()
            .Setup(fun p -> <@ p.GetPlugins() @>).Returns(Seq.empty)
            .Create()
    let pluginMgr1 = createPluginManager()
    let pluginMgr2 = createPluginManager()
    let pluginMgr3 = createPluginManager()
    let registrar, pipeline =
        let t = new System.Threading.Tasks.TaskCompletionSource<IRawMessageStanzaPlugin>() 
        Mock<IPluginManagerRegistrar>()
            .Setup(fun p -> <@ p.RegisterFor<IRawMessageStanzaPlugin>(any()) @>).Calls<IRawMessageStanzaPlugin>(fun arg -> t.SetResult arg)
            .Setup(fun r -> <@ r.CreateManagerFor<ILocalMessageSendStanzaPlugin>() @>).Returns(pluginMgr1)
            .Setup(fun r -> <@ r.CreateManagerFor<IRemoteMessageSendStanzaPlugin>() @>).Returns(pluginMgr2)
            .Setup(fun r -> <@ r.CreateManagerFor<ILocalMessageReceiveStanzaPlugin>() @>).Returns(pluginMgr3)
            .Create(),
        t.Task
    let statusChange = ObservableSource<_>()
    let imServerService = 
        Mock<IImServerService>()
            .Setup(fun s -> <@ s.StatusChanged @>).Returns(statusChange.AsObservable)
            .Create()
    let discoService = 
        Mock<IDiscoService>()
            .Create()
    let plugin = 
        IMServerMessagePlugin(
            serverApi, negotationService, addressing, 
            stanzas, runtimeConfig, config, registrar, imServerService, discoService)
    let pipeline = pipeline.Result
    
    member x.ChangeStatus (presence, isInitial) = statusChange.Next(presence, isInitial)
    member x.Plugin = plugin
    member x.Pipeline = pipeline
    member x.RemoteId = remoteId
    member x.Connections = connections
    
[<TestFixture>]
type ``Test-Yaaf-Xmpp-IM-Server-IMServerMessagePlugin: check XMPP_IM specification``() =
    inherit MyTestClass()
         
    [<Test>]
    member x.``Check that sending a message with AllToNonNegativeResources works`` () = 
        // setup test
        let clientConnections = 
            [ ("test@yaaf.de", [ ClientStatus.Online; ClientStatus.Offline ]); 
              ("contact@yaaf.de", [ ClientStatus.Online ]);
              ("other@yaaf.de", 
                [ ClientStatus.Online(*0*); ClientStatus.Interested(*1*); ClientStatus.Offline(*2*); 
                  ClientStatus.Available(*3*); ClientStatus.Create (false, Some -1y)(*4*);
                  ClientStatus.Create (false, Some 0y)(*5*); ClientStatus.Create (false, Some 1y) ])(*6*) ] |> dict
          
        let config =
            { 
                ResourceSendingStrategy = ResourceSendingStrategy.AllToNonNegativeResources
                OfflineMessagesEnabled = false
            }

        let env = new TestIMServerMessagePluginEnvironment(clientConnections, config, None)

        // Send the message
        let elem = 
            System.Xml.Linq.XElement.Parse (
                sprintf "<message xmlns='%s' from='%s' to='other@yaaf.de'><body>test</body></message>" KnownStreamNamespaces.clientNS env.RemoteId.FullId)
        let stanza = Parsing.parseMessageStanza KnownStreamNamespaces.clientNS elem
        let result = Pipeline.processPipeline "testPipeline" [env.Pipeline.ReceivePipeline] stanza |> Async.StartAsTask |> waitTask
        result.ProcessTask |> waitTask

        // check results
        for con in env.Connections.["other@yaaf.de"] do
            let imService = con.GetService<IImServerService>()
            let stanzaService = con.GetService<IXmlStanzaService>()
            match imService.CurrentPresenceData with
            | Some pres when pres.Priority.IsNone || pres.Priority.Value > -1y ->
                let expectedElem = 
                    System.Xml.Linq.XElement.Parse 
                        (sprintf "<message xmlns='%s' from='test@yaaf.de/res_0' to='other@yaaf.de'><body>test</body></message>" KnownStreamNamespaces.clientNS) 
                let expectedStanza = XmlStanzas.Parsing.parseStanzaElementNoError KnownStreamNamespaces.clientNS expectedElem
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (expectedStanza) @>, Times.Once)
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)
            | _ -> 
                Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)
 
    [<Test>]
    member x.``Check that offline messages work`` () = 
        // setup test
        let clientConnections = 
            [ ("test@yaaf.de", [ ClientStatus.Offline ]); 
              ("contact@yaaf.de", [ ClientStatus.Offline ]);
            ] |> dict
          
        let elem = 
            System.Xml.Linq.XElement.Parse (
                sprintf "<message xmlns='%s' from='contact@yaaf.de/resource' to='test@yaaf.de'><body>test</body></message>" KnownStreamNamespaces.clientNS)
        let savedStanza = Parsing.parseMessageStanza KnownStreamNamespaces.clientNS elem

        let config =
            { 
                ResourceSendingStrategy = ResourceSendingStrategy.AllToNonNegativeResources
                OfflineMessagesEnabled = true
            }
        let offlineStorage =
            Mock<IOfflineMessageStorage>()
                .Setup(fun o -> <@ o.StoreOfflineMessage(any(), any()) @>).Returns(Task.FromResult () :> System.Threading.Tasks.Task)
                .Setup(fun o -> <@ o.RetrieveOfflineMessages(any()) @>).Returns(Task.FromResult (Seq.singleton savedStanza))
                .Create()
        let env = new TestIMServerMessagePluginEnvironment(clientConnections, config, Some offlineStorage)

        // Send a message
        let elem = 
            System.Xml.Linq.XElement.Parse (
                sprintf "<message xmlns='%s' from='%s' to='contact@yaaf.de'><body>test</body></message>" KnownStreamNamespaces.clientNS env.RemoteId.FullId)
        let stanza = Parsing.parseMessageStanza KnownStreamNamespaces.clientNS elem
        let result = Pipeline.processPipeline "testPipeline" [env.Pipeline.ReceivePipeline] stanza |> Async.StartAsTask |> waitTask
        result.ProcessTask |> waitTask

        // check that it was not sent
        for con in env.Connections.["contact@yaaf.de"] do
            let stanzaService = con.GetService<IXmlStanzaService>()
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)

        // chat that the message was saved
        Mock.Verify(<@ offlineStorage.StoreOfflineMessage(any(), any()) @>, Times.Once)

        // check that we receive saved messages when we go online
        for con in env.Connections.["test@yaaf.de"] do
            let stanzaService = con.GetService<IXmlStanzaService>()
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Never)

        env.ChangeStatus(None, true)
        for con in env.Connections.["test@yaaf.de"] do
            let stanzaService = con.GetService<IXmlStanzaService>()
            Mock.Verify(<@ stanzaService.QueueStanza (any()) (any()) @>, Times.Once)