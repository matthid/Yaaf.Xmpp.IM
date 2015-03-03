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
open Yaaf.Xmpp.IM.DelayedDelivery

[<TestFixture>]
type ``Test-Yaaf-Xmpp-IM-DelayedDelivery: check parsing of DelayedDelivery``() =
    inherit MyTestClass()
         
    [<Test>]
    member x.``Check that parsing simple delayed message works`` () = 
        let elem = 
            System.Xml.Linq.XElement.Parse (
                sprintf "<message xmlns='%s' from='somebody@yaaf.de' to='other@yaaf.de'><body>test</body><delay xmlns='urn:xmpp:delay'
     from='capulet.com'
     stamp='2002-09-10T23:08:25Z'>Offline Storage</delay></message>" KnownStreamNamespaces.clientNS)
        let stanza = Parsing.parseMessageStanza KnownStreamNamespaces.clientNS elem
        test <@ Parsing.hasContentDelay stanza @>
        match Parsing.tryGetDelayItem stanza with
        | Some item ->
            test <@ item.Stamp = StreamData.DateTime.Parse("2002-09-10T23:08:25Z") @>
            test <@ item.From = Some (JabberId.Parse "capulet.com") @>
            test <@ item.Reason = Some ("Offline Storage") @>
        | None -> failwith "expected delay-item"

        
    [<Test>]
    member x.``Check that generating simple delayed message works`` () = 
        let elem = 
            System.Xml.Linq.XElement.Parse (
                sprintf "<message xmlns='%s' from='somebody@yaaf.de' to='other@yaaf.de'><body>test</body></message>" KnownStreamNamespaces.clientNS)
        let stanza = Parsing.parseMessageStanza KnownStreamNamespaces.clientNS elem
        let delayedStanza = 
            Parsing.addDelayInfo
                {
                    Stamp = StreamData.DateTime.Parse("2002-09-10T23:08:25Z")
                    From = Some (JabberId.Parse "capulet.com")
                    Reason = Some ("Offline Storage")
                } stanza
        let elem = Parsing.createStanzaElement KnownStreamNamespaces.clientNS delayedStanza
        
        let expectedElem = 
            System.Xml.Linq.XElement.Parse (
                sprintf "<message xmlns='%s' from='somebody@yaaf.de' to='other@yaaf.de'><body>test</body><delay xmlns='urn:xmpp:delay'
     from='capulet.com'
     stamp='2002-09-10T23:08:25Z'>Offline Storage</delay></message>" KnownStreamNamespaces.clientNS)

        test <@ Yaaf.Xml.Core.equalXNode expectedElem elem @>