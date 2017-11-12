#tool nuget:?package=NUnit.ConsoleRunner&version=3.4.0
//////////////////////////////////////////////////////////////////////
// ARGUMENTS
//////////////////////////////////////////////////////////////////////

var target = Argument("target", "Default");
var configuration = Argument("configuration", "Release");

//////////////////////////////////////////////////////////////////////
// PREPARATION
//////////////////////////////////////////////////////////////////////

// Define directories.
var buildDir = Directory("./RogueText.Examples/bin") + Directory(configuration);

//////////////////////////////////////////////////////////////////////
// TASKS
//////////////////////////////////////////////////////////////////////

Task("Clean")
    .Does(() =>
{
    CleanDirectory(buildDir);
});

Task("Restore-NuGet-Packages")
    .IsDependentOn("Clean")
    .Does(() =>
{
    NuGetRestore("./RogueText.sln");
});

Task("Download-File-References")
    .IsDependentOn("Clean")
    .Does(() =>
{
    var outputPath = File("./download/ProvidedTypes.fs");
    DownloadFile("https://raw.githubusercontent.com/fsprojects/FSharp.TypeProviders.SDK/f3f821c6404af37a49656c87a550b7eaf30454d9/src/ProvidedTypes.fs", outputPath);
});

Task("Build")
    .IsDependentOn("Restore-NuGet-Packages")
    .IsDependentOn("Download-File-References")
    .Does(() =>
{
    // Use MSBuild
    MSBuild("./RogueText.sln", settings =>
    settings.SetConfiguration(configuration));
});

//////////////////////////////////////////////////////////////////////
// TASK TARGETS
//////////////////////////////////////////////////////////////////////

Task("Default")
    .IsDependentOn("Build");

//////////////////////////////////////////////////////////////////////
// EXECUTION
//////////////////////////////////////////////////////////////////////

RunTarget(target);
