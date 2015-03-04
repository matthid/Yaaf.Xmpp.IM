// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Test.Yaaf.Xmpp.IM
open FsUnit
open NUnit.Framework
open Yaaf.Helper
open Yaaf.TestHelper
open Yaaf.Xmpp
open Yaaf.Xmpp.IM
open Swensen.Unquote

[<AbstractClass>]
type RosterStoreTests() =
    inherit MyTestClass()
    let mutable msgStore = Unchecked.defaultof<_>
    abstract member CreateRosterStore : unit -> (Yaaf.Xmpp.IM.Server.IRosterStore)

    override x.Setup () = 
        // Setup DataDirectory for databases
        System.AppDomain.CurrentDomain.SetData(
            "DataDirectory", 
            System.AppDomain.CurrentDomain.BaseDirectory)

        msgStore <- x.CreateRosterStore()
        base.Setup()

    override x.TearDown() = 
        msgStore <- Unchecked.defaultof<_>
        base.TearDown()
                
    [<Test>]
    member x.``Check that rosters exists on demand`` () = 
        msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org")
        |> should not' (equal null) 
        msgStore.GetRoster (JabberId.Parse "testuser1@nunit.org")
        |> should not' (equal null) 
        msgStore.GetRoster (JabberId.Parse "testuser2@nunit.org")
        |> should not' (equal null) 
        
        msgStore.ExistsUser (JabberId.Parse "testuser0@nunit.org")
        |> should be True
        msgStore.ExistsUser (JabberId.Parse "testuser1@nunit.org")
        |> should be True
        msgStore.ExistsUser (JabberId.Parse "testuser2@nunit.org")
        |> should be True
        
    [<Test>]
    member x.``Check presence is not null`` () = 
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org") |> Task.result
        roster.GetCurrentPresence() |> Task.result |> should not' (equal null)
        
    [<Test>]
    member x.``Check that we return the same store independent of resource`` () = 
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org/test1") |> Task.result
        
        let item = RosterItem.CreateEmpty (JabberId.Parse "testuser1@nunit.org")
        let ver = roster.UpdateOrAddRosterItem false item |> Task.result
        
        let roster2 = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org") |> Task.result
        roster2.GetCurrentRosterVersion() |> Task.result |> should be (equal ver)
        let item2 = roster.GetItem (JabberId.Parse "testuser1@nunit.org") |> Task.result
        item2.IsSome |> should be True
        item2.Value |> should be (equal item)
        
        let roster2 = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org/test2") |> Task.result
        roster2.GetCurrentRosterVersion() |> Task.result |> should be (equal ver)
        let item2 = roster.GetItem (JabberId.Parse "testuser1@nunit.org") |> Task.result
        item2.IsSome |> should be True
        item2.Value |> should be (equal item)

    [<Test>]
    member x.``Check that we get the same item independent of resource`` () = 
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org") |> Task.result
        let itemJid1 = JabberId.Parse "testuser1@nunit.org/test1"
        let itemJid2 = JabberId.Parse "testuser1@nunit.org/test2"
        let itemJid3 = JabberId.Parse "testuser1@nunit.org"
        let item = { RosterItem.CreateEmpty itemJid1 with Ask = Some AskType.Subscribe; Name = Some "Hans" }
        let ver = roster.UpdateOrAddRosterItem false item |> Task.result
        
        let currentItem = roster.GetItem itemJid2 |> Task.result
        test <@ currentItem.IsSome @>
        let currentItem = currentItem.Value
        test <@ currentItem.Ask.IsSome && currentItem.Ask.Value = AskType.Subscribe @>
        test <@ currentItem.Name.IsSome && currentItem.Name.Value = "Hans" @>
        
        let currentItem = roster.GetItem itemJid3 |> Task.result
        test <@ currentItem.IsSome @>
        let currentItem = currentItem.Value
        test <@ currentItem.Ask.IsSome && currentItem.Ask.Value = AskType.Subscribe @>
        test <@ currentItem.Name.IsSome && currentItem.Name.Value = "Hans" @>

    [<Test>]
    member x.``Check that we can save an item with group`` () = 
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org/test1") |> Task.result
        let itemJid = JabberId.Parse "testuser1@nunit.org"
        let item = { RosterItem.CreateEmpty itemJid with Groups = ["test1"; "test2" ] }
        let ver = roster.UpdateOrAddRosterItem false item |> Task.result
        
        let currentItem = roster.GetItem itemJid |> Task.result
        currentItem.IsSome |> should be True
        currentItem.Value.Groups |> List.length |> should be (equal 2)

        let changeItem = { item with Groups = ["test0"] }
        let ver = roster.UpdateOrAddRosterItem true changeItem |> Task.result
        let currentItem = roster.GetItem itemJid |> Task.result
        currentItem.IsSome |> should be True
        currentItem.Value.Groups |> List.length |> should be (equal 1)
        
    [<Test>]
    member x.``Check that we can update an item to a group`` () = 
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org/test1") |> Task.result
        let itemJid = JabberId.Parse "testuser1@nunit.org"
        let item = RosterItem.CreateEmpty itemJid
        let ver = roster.UpdateOrAddRosterItem false item |> Task.result
        
        let currentItem = roster.GetItem itemJid |> Task.result
        currentItem.IsSome |> should be True
        currentItem.Value.Groups |> List.length |> should be (equal 0)

        let changeItem = { item with Groups = ["test0"] }
        let ver = roster.UpdateOrAddRosterItem true changeItem |> Task.result
        let currentItem = roster.GetItem itemJid |> Task.result
        currentItem.IsSome |> should be True
        currentItem.Value.Groups |> List.length |> should be (equal 1)
    
    [<Test>]
    member x.``Check that we can add an item with group via update`` () = 
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org/test1") |> Task.result
        let itemJid = JabberId.Parse "testuser1@nunit.org"
        let item = RosterItem.CreateEmpty itemJid
        let changeItem = { item with Groups = ["test0"] }
        let ver = roster.UpdateOrAddRosterItem true changeItem |> Task.result
        let currentItem = roster.GetItem itemJid |> Task.result
        currentItem.IsSome |> should be True
        currentItem.Value.Groups |> List.length |> should be (equal 1)

    // TODO: Fix and activate me again
    //[<Test>]
    //member x.``Check that saving items works (including versioning)`` () = 
    //    let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org")
    //    let rosterVer = roster.CurrentRosterVersion
    //    let item = RosterItem.CreateEmpty (JabberId.Parse "testuser1@nunit.org")
    //
    //    // make sure this item doesn't exist
    //    roster.RosterSize |> should be (equal 0)
    //    roster.GetItems() |> List.exists (fun l -> l.Jid.FullId = item.Jid.FullId) |> should be False
    //    roster.GetItem(item.Jid).IsSome |> should be False
    //    
    //    // add an item
    //    let addVer = roster.UpdateOrAddRosterItem false (item)
    //    rosterVer |> should not' (equal addVer)
    //    roster.CurrentRosterVersion |> should be (equal addVer)
    //    roster.RosterSize |> should be (equal 1)
    //
    //    roster.GetItems() |> List.exists (fun l -> l.Jid.FullId = item.Jid.FullId) |> should be True
    //    roster.GetItem(item.Jid).IsSome |> should be True
    //
    //    let changes = roster.ChangesSince(rosterVer)
    //    // changes should contain exactly the added item
    //    changes.Length |> should be (equal 1)
    //    let change, ver = changes.Head
    //    ver |> should be (equal addVer)
    //    let changeItem = 
    //        match change with
    //        | DeleteItem i -> 
    //            Assert.Fail ("Expected SetItem")
    //            failwith "expected setitem"
    //        | SetItem i -> i
    //    changeItem.Jid.FullId |> should be (equal item.Jid.FullId)
    //
    //    
    //    // change/update an item
    //    let item = { item with Subscription = Some SubscriptionType.Both }
    //    let changeVer = roster.UpdateOrAddRosterItem true (item)
    //    rosterVer |> should not' (equal changeVer)
    //    roster.CurrentRosterVersion |> should be (equal changeVer)
    //    roster.RosterSize |> should be (equal 1)
    //
    //    roster.GetItems() |> List.exists (fun l -> l.Jid.FullId = item.Jid.FullId) |> should be True
    //    roster.GetItem(item.Jid).IsSome |> should be True
    //
    //    let changes = roster.ChangesSince(rosterVer)
    //    // changes should contain exactly the added/changed item
    //    changes.Length |> should be (equal 1)
    //    let change, ver = changes.Head
    //    ver |> should be (equal changeVer)
    //    let changeItem = 
    //        match change with
    //        | DeleteItem i -> 
    //            Assert.Fail ("Expected SetItem")
    //            failwith "expected setitem"
    //        | SetItem i -> i
    //    changeItem.Jid.FullId |> should be (equal item.Jid.FullId)
    //
    //    // delete an item
    //    let delVer = roster.DeleteItem(item.Jid)
    //    rosterVer |> should not' (equal delVer)
    //    addVer |> should not' (equal delVer)
    //    changeVer |> should not' (equal delVer)
    //    roster.CurrentRosterVersion |> should be (equal delVer)
    //    roster.RosterSize |> should be (equal 0)
    //
    //    roster.GetItems() |> List.exists (fun l -> l.Jid.FullId = item.Jid.FullId) |> should be False
    //    roster.GetItem(item.Jid).IsSome |> should be False
    //
    //    // change should contain the deleted item
    //    let changes = roster.ChangesSince(changeVer)
    //    changes.Length |> should be (equal 1)
    //    let change, ver = changes.Head
    //    ver |> should be (equal delVer)
    //    let changeItem = 
    //        match change with
    //        | DeleteItem i -> i
    //        | SetItem i -> 
    //            Assert.Fail ("Expected DeleteItem")
    //            failwith "expected DeleteItem"
    //    changeItem.FullId |> should be (equal item.Jid.FullId)
    //    
    //    // TODO: changes should be "cleaned up" 
    //    let changes = roster.ChangesSince(rosterVer)
    //    if (changes.Length > 0) then
    //        if changes.Length = 1 then
    //            warn ("Delete Push should not get sent!")
    //        else
    //        changes.Length |> should be (equal 2)
    //        warn ("changes should get cleaned up (the correct behaviour is correct but inefficient)")
    //    // if length is zero we are good


        
    [<Test>]
    member x.``Check that saving/updating subscriptionrequest works`` () =
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org") |> Task.result
        (roster.RetrieveStoredSubscriptionRequests() |> Task.result).Length |> should be (equal 0) 
        let stanza = 
            Parsing.createPresenceElement 
                (Some "idtest") 
                (None)
                (Some (JabberId.Parse "to@nunit.org"))
                PresenceProcessingType.SubscriptionRequest
        let from = JabberId.Parse "from@nunit.org"
        roster.StoreSubscriptionRequest (from) (stanza) |> Task.wait
        (roster.RetrieveStoredSubscriptionRequests() |> Task.result).Length |> should be (equal 1)

        let changedStanza = 
            Parsing.createPresenceElement 
                (Some "blub") 
                (None)
                (Some (JabberId.Parse "to@nunit.org"))
                PresenceProcessingType.SubscriptionRequest
        roster.StoreSubscriptionRequest (from) (changedStanza) |> Task.wait
        (roster.RetrieveStoredSubscriptionRequests() |> Task.result).Length |> should be (equal 1)

        roster.RemoveSubscriptionRequest(from) |> Task.wait
        (roster.RetrieveStoredSubscriptionRequests() |> Task.result).Length |> should be (equal 0)

        
    [<Test>]
    member x.``Check that saving/updating presence works`` () =
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org") |> Task.result
        roster.GetCurrentPresence() |> Task.result |> should not' (equal null)
        
        let statusOffline = PresenceStatus.SetStatusUnavailable [];
        let statusOnline = PresenceStatus.SetStatus { PresenceData.Empty with Status = [] };
        roster.SetCurrentPresence statusOffline |> Task.wait
        roster.GetCurrentPresence() |> Task.result |> should not' (equal null)
        roster.SetCurrentPresence statusOnline |> Task.wait
        roster.GetCurrentPresence() |> Task.result |> should not' (equal null)

        
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org/test") |> Task.result
        roster.GetCurrentPresence() |> Task.result |> should be (equal statusOnline)

        (fun () -> roster.SetCurrentPresence Unchecked.defaultof<_> |> Task.wait)
            |> should throw typeof<System.ArgumentNullException>


    [<Test>]
    member x.``Check that saving status works`` () =
        let roster = msgStore.GetRoster (JabberId.Parse "testuser0@nunit.org") |> Task.result
        let status = PresenceStatus.SetStatusUnavailable []
        roster.SetCurrentPresence status |> Task.wait
