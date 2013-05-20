namespace Minesweeper.Tests
open NUnit.Framework

[<TestFixture>]
type AcceptanceTests() =

    [<Test>]
    [<TestCase(1)>]
    member this.Test(testNum: int) =
        use sr = AcceptanceTests.getTestInputReader(testNum)
        let sw = new System.IO.StringWriter()
        Program.readAndWrite sr sw |> ignore
        let actual = sw.GetStringBuilder().ToString()
        let expected = AcceptanceTests.getTestOutputString(testNum)
        Assert.AreEqual(expected, actual)

    static member getTestInputReader (testNumber: int) =
        sprintf "input%d.txt" testNumber |> AcceptanceTests.getResourceReader

    static member getTestOutputString (testNumber: int) =
        let resourceName = sprintf "output%d.txt" testNumber
        use sr = AcceptanceTests.getResourceReader resourceName
        sr.ReadToEnd()

    static member getResourceReader (resourceName: string) =
        let resourceStream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream resourceName
        new System.IO.StreamReader(resourceStream)
    
