// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// Implementation of XEP-0203: Delayed Delivery (http://xmpp.org/extensions/xep-0203.html)
namespace Yaaf.Xmpp.IM.DelayedDelivery

open Yaaf.Xmpp.IM.DelayedDelivery
open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.IM
open Yaaf.Logging
open Yaaf.Xmpp.IM.Server

[<AutoOpen>]
module StanzaExtensions =
    type IStanza with
        member x.Delivery
            with get () =
                x.GetCacheData<DelayInfo> (Parsing.tryGetDelayItem)
        member x.AddDeliveryInfo v =
                x.AddContent (Parsing.createDelayInfoElem v) 
        
module Stanza =
    let getDelivery (s:IStanza) = s.Delivery
    let addDelivery (d:DelayInfo) (s:IStanza) = s.AddDeliveryInfo d

module Delay =
    let getNow reason (stanza: Stanza<_>) =
        { Reason = reason
          From = stanza.Header.From
          Stamp = System.DateTime.Now }
    

    /// adds Delayed information to the given IOfflineMessageStorage
    let addDelayedToOfflineStorage (storage:IOfflineMessageStorage) = 
        { new IOfflineMessageStorage with
            member x.StoreOfflineMessage (jid, msgStanza) =
                storage.StoreOfflineMessage (jid, msgStanza |> Stanza.addDelivery (getNow None msgStanza) |> Stanza.As<MessageData>)
            member x.RetrieveOfflineMessages (jid) =
                storage.RetrieveOfflineMessages (jid)
        }