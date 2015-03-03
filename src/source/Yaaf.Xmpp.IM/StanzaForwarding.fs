// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// Implementation of XEP-0297: Stanza Forwarding (http://xmpp.org/extensions/xep-0297.html)
namespace Yaaf.Xmpp.IM.StanzaForwarding

open Yaaf.Xmpp.IM.DelayedDelivery
open Yaaf.Helper
open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.IM
open Yaaf.Logging
open Yaaf.Xmpp.ServiceDiscovery

[<AutoOpen>]
module StanzaExtensions =
    type IStanza with
        member x.Forward
            with get () =
                x.GetCacheData<ForwardInfo> (Parsing.tryGetForwardItem)
        member x.AddForwardInfo v =
                x.AddContent (Parsing.createForwardedInfoElem v) 
        
module Stanza =
    let getForward (s:IStanza) = s.Forward
    let addForward (d:ForwardInfo) (s:IStanza) = s.AddForwardInfo d

/// A simple plugin to announce the forward feature in service discovery
type ForwardingPlugin (disco : IDiscoService) =
    do
        disco.RegisterFeatureItem (None, { Var = "urn:xmpp:forward:0"})
  
    interface IXmppPlugin with
        member __.Name = "ForwardingPlugin"
        member x.PluginService = Service.None