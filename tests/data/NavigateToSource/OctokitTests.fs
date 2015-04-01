module OctokitTests

// https://github.com/octokit/octokit.net/
let dt = Octokit.Helpers.UnixTimestampExtensions.FromUnixTime 1425868070L
let github = Octokit.GitHubClient(Octokit.ProductHeaderValue "MyAmazingApp")
let a = github.Activity.Events.GetAll()
()
