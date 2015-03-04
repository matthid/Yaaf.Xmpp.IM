// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
open FsUnit
open NUnit.Framework
open Yaaf.Helper
open Yaaf.TestHelper
open Test.Yaaf.Xmpp
open Yaaf.Xmpp
open Yaaf.Xmpp.IM.Server
open Yaaf.Xmpp.IM
open Swensen.Unquote
open Foq

[<TestFixture>]
type ``Test-Yaaf-Xmpp-IM-Server-IRosterStore: check interface specification against memory``() =
    inherit RosterStoreTests()

    override x.CreateRosterStore () = new MemoryRosterStore() :> Yaaf.Xmpp.IM.Server.IRosterStore
