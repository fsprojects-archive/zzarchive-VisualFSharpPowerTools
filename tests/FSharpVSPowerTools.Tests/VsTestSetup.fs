
namespace FSharpVSPowerTools.Tests

open FSharpVSPowerTools
open FSharpVSPowerTools.ProjectSystem
open NUnit.Framework

[<SetUpFixture>]
type VsTestSetup() =
    [<OneTimeSetUp>]
    member __.SetUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true
        Logger.GlobalServiceProvider <- VsTestBase.GlobalServiceProvider
