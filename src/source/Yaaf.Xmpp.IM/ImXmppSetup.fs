// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.IM

open Yaaf.Xmpp
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.Runtime.Features

#if CSHARP_EXTENSIONS
[<System.Runtime.CompilerServices.Extension>]
#endif
module XmppSetup =
    let setupImCore (runtime : XmppRuntime) =
        let mgr = runtime.PluginManager
        let featureService = mgr.GetPluginService<IStreamFeatureService>()
        let featureMgr = featureService.FeatureManager
        featureMgr.RegisterPlugin<RosterVerFeature>()
        featureMgr.RegisterPlugin<PreApprovalFeature>()
        featureMgr.RegisterPlugin<SessionFeature>()

    let setupImCoreClient (runtime:XmppRuntime) = 
        setupImCore runtime
        let mgr = runtime.PluginManager
        mgr.RegisterPlugin<MessagePlugin>()

    /// Adds the ServiceDiscovery plugin
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddServiceDiscovery (setup: ClientSetup) = 
        let setupDiscovery (runtime:XmppRuntime) = 
            let mgr = runtime.PluginManager
            mgr.RegisterPlugin<ServiceDiscovery.DiscoPlugin>()
        setup
        |> XmppSetup.addHelper ignore setupDiscovery
    let addServiceDiscovery setup = AddServiceDiscovery(setup)
    
    /// Adds MessagePlugin with all dependencies (besides CoreClient and Disco) 
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddMessagingClientCore (setup: ClientSetup) = 
        setup
        |> XmppSetup.addHelper ignore setupImCoreClient
    let addMessagingClientCore setup = AddMessagingClientCore(setup)
    
    /// Adds MessagePlugin with all dependencies
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddMessagingClient (setup: ClientSetup) = 
        setup
        |> XmppSetup.addCoreClient
        |> addServiceDiscovery
        |> addMessagingClientCore
    let addMessagingClient setup = AddMessagingClient(setup)

namespace Yaaf.Xmpp.IM.Server

open Yaaf.Xmpp
open Yaaf.Xmpp.Server
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.IM

#if CSHARP_EXTENSIONS
[<System.Runtime.CompilerServices.Extension>]
#endif
module XmppSetup =
    let setupImCoreServer (runtime:XmppRuntime) = 
        //XmppSetup.setupImCore runtime
        let mgr = runtime.PluginManager
        mgr.RegisterPlugin<MessagePlugin>()
        mgr.RegisterPlugin<IMServerPlugin>()
        
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let internal SetIMServerConfig (setup, config:IImServerConfig) =
        XmppSetup.SetConfig<ImServerConfig,_> (setup, config)
    let internal setIMServerConfig config setup = SetIMServerConfig(setup, config)

    
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let internal SetServerMessagePluginConfig (setup, config:IImServerMessagePluginConfig) =
        XmppSetup.SetConfig<ImServerMessagePluginConfig,_> (setup, config)
    let internal setServerMessagePluginConfig config setup = SetServerMessagePluginConfig(setup, config)

    /// Adds MessagePlugin with all dependencies (besides CoreClient and Disco) 
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddIMServerCore (setup: ClientSetup, config) = 
        setup
        |> setIMServerConfig config
        |> XmppSetup.addHelper ignore setupImCoreServer
    let addIMServerCore config setup = AddIMServerCore(setup, config)
    
    let internal setupMessagePipeline (runtime:XmppRuntime) = 
        //XmppSetup.setupImCore runtime
        let mgr = runtime.PluginManager
        mgr.RegisterPlugin<MessagePipelinePlugin>()
        
    let internal setupMessagePlugin (runtime:XmppRuntime) = 
        //XmppSetup.setupImCore runtime
        let mgr = runtime.PluginManager
        mgr.RegisterPlugin<IMServerMessagePlugin>()
    
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddMessagePipeline (setup: ClientSetup) = 
        setup
        |> XmppSetup.addHelper ignore setupMessagePipeline
    let addMessagePipeline setup = AddMessagePipeline(setup)
    
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let AddMessagePlugin (setup: ClientSetup, config) = 
        setup
        |> setServerMessagePluginConfig config
        |> XmppSetup.addHelper ignore setupMessagePlugin
    let addMessagePlugin config setup = AddMessagePlugin(setup, config)


    let setupMessagingFeatures (setup: ClientSetup) = 
        setup
        |> XmppSetup.addHelper ignore XmppSetup.setupImCore
        
#if CSHARP_EXTENSIONS
[<System.Runtime.CompilerServices.Extension>]
#endif
module XmppServerSetup =
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let addDiscoPlugin setup =
        setup
        |> XmppServerSetup.addToAllStreams XmppSetup.addServiceDiscovery
        
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let addIMPlugin config setup =
        setup
        |> XmppServerSetup.addToKnownConfig
            (function
             | ConfigType.C2SConfig -> true
             | _ -> false)
            (XmppSetup.setupMessagingFeatures)
        |> XmppServerSetup.addToAllStreams (XmppSetup.addIMServerCore config)

        
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let addMessagePipeline setup =
        setup
        |> XmppServerSetup.addToAllStreams (XmppSetup.addMessagePipeline)

        
#if CSHARP_EXTENSIONS
    [<System.Runtime.CompilerServices.Extension>]
#endif
    let addMessagePlugin config setup =
        setup
        |> XmppServerSetup.addToAllStreams (XmppSetup.addMessagePlugin config)