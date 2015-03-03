// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM

open System.IO
open NUnit.Framework
open FsUnit
open Test.Yaaf.Xml.XmlTestHelper
open Yaaf.Xmpp
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.IM
open System.Threading.Tasks
open Yaaf.IO
open Yaaf.TestHelper
open Yaaf.Xmpp.XmlStanzas
open Test.Yaaf.Xmpp

[<TestFixture>]
type ``Test-Yaaf-Xmpp-IM-Parsing: Test that rosterparsing works``() as this =
    inherit XmlStanzaParsingTestClass()
    let rosterTest stanzaString (info:RawStanza) (elem:RosterAction) = 
        let newStanza = Parsing.createRosterElement info.Header.Id.Value (info.Header.From) (info.Header.To) elem
        this.GenericTest Parsing.rosterContentGenerator stanzaString newStanza

// Test roster messages

    [<Test>]
    member this.``Check that we can parse & write Roster message: roster get`` () = 
        let stanza = "<iq from='juliet@example.com/balcony'
       id='bv1bs71f'
       type='get'>
    <query xmlns='jabber:iq:roster'/>
  </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.GetRoster _ -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.GetRoster"
        rosterTest stanza info elem
        ()
    [<Test>]
    member this.``Check that we can parse & write Roster message: roster result`` () = 
        let stanza = "<iq id='bv1bs71f'
       to='juliet@example.com/chamber'
       type='result'>
    <query xmlns='jabber:iq:roster' ver='ver7'>
      <item jid='nurse@example.com'/>
      <item jid='romeo@example.net'/>
    </query>
  </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterData _ -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterData"
        rosterTest stanza info elem
        ()
    [<Test>]
    member this.``Check that we can parse & write Roster message: roster set`` () = 
        let stanza = "<iq from='juliet@example.com/balcony'
       id='rs1'
       type='set'>
    <query xmlns='jabber:iq:roster'>
      <item jid='nurse@example.com'/>
    </query>
  </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterChange ((RosterChange.SetItem _),_) -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterChange"
        rosterTest stanza info elem
        ()
    [<Test>]
    member this.``Check that we can parse & write Roster message: roster set with ask`` () = 
        let stanza = "<iq from='juliet@example.com/balcony'
       id='rs1'
       type='set'>
    <query xmlns='jabber:iq:roster'>
      <item ask='subscribe' jid='nurse@example.com'/>
    </query>
  </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterChange ((RosterChange.SetItem _),_) -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterChange"
        rosterTest stanza info elem
        ()
    [<Test>]
    member this.``Check that we can parse & write Roster message: roster set with approved`` () = 
        let stanza = "<iq from='juliet@example.com/balcony'
       id='rs1'
       type='set'>
    <query xmlns='jabber:iq:roster'>
      <item approved='true' jid='nurse@example.com'/>
    </query>
  </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterChange ((RosterChange.SetItem _),_) -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterChange"
        rosterTest stanza info elem
        ()


    [<Test>]
    member this.``Check that we can parse & write Roster message: roster set with to and from`` () = 
        let stanza = "<iq from='juliet@example.com/balcony' to='example.com'
       id='rs1'
       type='set'>
    <query xmlns='jabber:iq:roster'>
      <item jid='nurse@example.com'/>
    </query>
  </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterChange  ((RosterChange.SetItem _),_) -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterChange"
        rosterTest stanza info elem
        ()

    [<Test>]
    member this.``Check that we can parse & write Roster message: roster push`` () = 
        let stanza = "<iq id='a78b4q6ha463'
       to='juliet@example.com/chamber'
       type='set'>
    <query xmlns='jabber:iq:roster'>
      <item jid='nurse@example.com'/>
    </query>
  </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterChange ((RosterChange.SetItem _),_)  -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterChange"
        rosterTest stanza info elem
        ()
    [<Test>]
    member this.``Check that we can parse & write Roster message: roster set with groups`` () = 
        let stanza = "<iq from='juliet@example.com/balcony'
       id='di43b2x9'
       type='set'>
     <query xmlns='jabber:iq:roster'>
       <item jid='romeo@example.net'
             name='Romeo'>
         <group>Friends</group>
         <group>Lovers</group>
       </item>
     </query>
   </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterChange ((RosterChange.SetItem _),_) -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterChange"
        rosterTest stanza info elem
        ()
        
    [<Test>]
    member this.``Check that we can parse & write Roster message: roster delete`` () = 
        let stanza = "<iq from='juliet@example.com/balcony'
       id='hm4hs97y'
       type='set'>
     <query xmlns='jabber:iq:roster'>
       <item jid='nurse@example.com'
             subscription='remove'/>
     </query>
   </iq>"
        let info = this.Test stanza
        info |> Parsing.isContentRoster |> should be True
        let elem = Parsing.parseContentRoster info
        match elem with
        | RosterAction.RosterChange ((RosterChange.DeleteItem _),_) -> ()
        | _ -> Assert.Fail "parsed should be RosterAction.RosterChange"
        rosterTest stanza info elem
        ()