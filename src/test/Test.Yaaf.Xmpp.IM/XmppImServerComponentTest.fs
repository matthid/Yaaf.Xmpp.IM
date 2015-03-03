// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
(*
open FsUnit
open NUnit.Framework
open Yaaf.Helper
open Yaaf.TestHelper
open Test.Yaaf.Xmpp
open Yaaf.Xmpp.Features
open Yaaf.Xmpp
open Yaaf.Xmpp.Negotiation
open Yaaf.Xmpp.Handler
open Yaaf.Xmpp.IM.MessageArchiving.Core
open Yaaf.Xmpp.IM.Data
open Yaaf.Xmpp.IM.Core
open Yaaf.Xmpp.IM.Server

[<TestFixture>]
type XmppIMServerComponentTestClass() =
    inherit XmppServerComponentTestClass()
    let mutable store = Unchecked.defaultof<_>
        
    override x.ServerConfig streamType = 
        let baseFun =  base.ServerConfig streamType
        fun config ->
            let config = baseFun config
            config.RemovePlugin Yaaf.Xmpp.Server.ServerPlugin.XmppServerPlugin
            config.RemovePlugin Yaaf.Xmpp.ClientPlugin.XmppClientPlugin
            config.AddPlugin Yaaf.Xmpp.IM.Server.IMServerPlugin
            config.MessagingStore <- store
            if streamType.OnClientStream then // c2s 
                config.AddFeature Yaaf.Xmpp.Features.Session.SessionFeature
                config.AddFeature Yaaf.Xmpp.Features.Roster.RosterVerFeature
            config
    override x.ClientConfig instanceNum = 
        let baseFun =  base.ClientConfig instanceNum
        fun config ->
            let config = baseFun config
            if not <| x.IsComponent instanceNum then
                config.AddFeature Yaaf.Xmpp.Features.Session.SessionFeature
                config.AddFeature Yaaf.Xmpp.Features.Roster.RosterVerFeature
            config.RemovePlugin Yaaf.Xmpp.Server.ServerPlugin.XmppServerPlugin
            config.RemovePlugin Yaaf.Xmpp.ClientPlugin.XmppClientPlugin
            config.AddPlugin Yaaf.Xmpp.IM.Core.MessagePlugin
            config

    override x.Setup () = 
        let userStore = MemoryRosterStore() :> RosterStore
        //let roster0 = userStore.GetRoster (JabberId.Parse "test0@nunit.org")
        //roster0.UpdateOrAddRosterItem false { RosterItem.CreateEmpty (JabberId.Parse "test1@nunit.org") with Subscription = Some Both } |> ignore
        //let roster1 = userStore.GetRoster (JabberId.Parse "test1@nunit.org")
        //roster1.UpdateOrAddRosterItem false { RosterItem.CreateEmpty (JabberId.Parse "test0@nunit.org") with Subscription = Some Both } |> ignore
        store <- userStore
        base.Setup()

    override x.TearDown() = 
        store <- Unchecked.defaultof<_>
        base.TearDown()
        
        
    [<Test>]
    member x.``Check if we can send a simple message stanza to another entity (namespaces work)``() =
        let xmppClient_0 =  x.XmppClients 0
        let xmppClient_1 =  x.XmppClients 1
        
        let receiver_0 = NUnitHelper.StanzaReceiver xmppClient_0
        let result_0 =  receiver_0 |> NUnitHelper.readNext |> Async.StartAsTask
        
        let receiver_1 = NUnitHelper.StanzaReceiver xmppClient_1
        let result_1 = receiver_1 |> NUnitHelper.readNext |> Async.StartAsTask
        // Wait for negotiation events, so that server can actually deliver the message
        xmppClient_0.WriteRaw "<finishnegotiation/>" |> x.WaitProcess // force end of negotiation procedure
        x.ServerNegotiatedClient 0 |> waitTaskI "client 0 connected to server"
        
        xmppClient_1
            .WriteRaw "<message to='test0@nunit.org' from='blub@comp0.nunit.org' type='chat'><body>test</body></message>" |> x.WaitProcess
        x.ServerNegotiatedClient 1 |> waitTaskI "client 1 connected to server"
        xmppClient_0
            .WriteRaw "<message from='test0@nunit.org' to='blub@comp0.nunit.org' type='chat'><body>test</body></message>" |> x.WaitProcess
        let stanza, receiver = result_0 |> waitTaskI "result"
        let content = Parsing.parseContentMessage stanza
        content.Body.Length |> should be (equal 1)

        let stanza, receiver = result_1 |> waitTaskI "result"
        let content = Parsing.parseContentMessage stanza
        content.Body.Length |> should be (equal 1)

        
*)