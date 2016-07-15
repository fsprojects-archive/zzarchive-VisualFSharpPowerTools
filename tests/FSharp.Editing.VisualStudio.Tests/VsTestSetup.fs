namespace FSharp.Editing.VisualStudio.Tests

open NUnit.Framework
open FSharp.Editing.VisualStudio

[<SetUpFixture>]
type VsTestSetup() =
    [<OneTimeSetUp>]
    member __.SetUp() =
        TestUtilities.AssertListener.Initialize()
        DocumentEventListener.SkipTimerDelay <- true
        Logger.GlobalServiceProvider <- VsTestBase.GlobalServiceProvider
