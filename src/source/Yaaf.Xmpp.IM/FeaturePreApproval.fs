// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.Runtime.Features
(* 
    XMPP.IM 3.4.  Pre-Approving a Subscription Request
    If a user has not received a subscription request from a contact, the user can "pre-approve" such a request 
    so that it will be automatically approved by the user's server.

    Support for subscription pre-approvals is OPTIONAL on the part of clients and servers. If a server supports subscription pre-approvals, 
    then it MUST advertise the following stream feature during stream negotiation.

        <sub xmlns='urn:xmpp:features:pre-approval'/>

    The subscription pre-approval stream feature is merely informative and therefore is never mandatory-to-negotiate. 
*)
open FSharpx.Collections
open Yaaf.Logging
open Yaaf.Helper // Equals pattern matching
// Note that we depend on XmlStanzas to be present

module ParsingPreApproval =
    open System.Xml.Linq
    open Yaaf.Xml
    let preApprovalNs = "urn:xmpp:features:pre-approval"
    // element within "feature" advertisement
    let checkIfPreApproval (elem:XElement) = 
        match elem.Name.NamespaceName, elem.Name.LocalName with
        | (Equals preApprovalNs), "sub" -> true
        | _ -> false

    let createAnnouncementElement () = 
        getXName "sub" preApprovalNs |> getXElem

open ParsingPreApproval
    
type PreApprovalFeature (sasl : ISaslService) = 
    interface IStreamFeatureHandler with 
        member __.PluginService = Seq.empty
        member __.GetState (featureList) = 
            if not sasl.IsRemoteAuthenticated then
                Unavailable
            else
                let maybeFound =
                    featureList 
                    |> Seq.choose(fun elem -> if checkIfPreApproval elem then Some elem else None)
                    |> Seq.tryHead
                match maybeFound with
                | None -> 
                    Unavailable
                | Some (elem) -> 
                    FeatureInfo.NegotiatedOrAnnounceOnly elem

        /// Used when client selects feature
        member __.InitializeFeature () =
            async {
                // Must not be selected!
                failwith "NegotiatedOrAnnounceOnly featues can't be selected!"
                return ()
            } |> Log.TraceMe

    // Server
        // Features can disable itself by returning None
        member __.CreateAnnounceFeatureElement () = 
            if not sasl.IsRemoteAuthenticated then
                FeatureInfo.Unavailable
            else
                // announce only
                FeatureInfo.NegotiatedOrAnnounceOnly (createAnnouncementElement ())

        /// Used so that the receiving entity can select the selected feature
        member __.IsFeatureSelected _ = false // isRosterVerElement config.StreamNamespace elem
        member __.HandleReceivingCommunication (_) =
            async {
                failwith "NegotiatedOrAnnounceOnly featues can't be selected!"
                return ()
            } 
            |> Log.TraceMe

