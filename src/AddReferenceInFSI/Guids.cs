// Guids.cs
// MUST match guids.h
using System;

namespace TaoLiu.AddReferenceInFSI
{
    static class GuidList
    {
        public const string guidAddReferenceInFSIPkgString = "082779d0-1be3-40a1-862a-be8c9133b1fb";
        public const string guidAddReferenceInFSICmdSetString = "8c9a49dd-2d34-4d18-905b-c557692980be";

        public static readonly Guid guidAddReferenceInFSICmdSet = new Guid(guidAddReferenceInFSICmdSetString);
    };
}