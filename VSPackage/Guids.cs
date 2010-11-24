﻿// Guids.cs
// MUST match guids.h
using System;

namespace MicrosoftResearch.VSPackage
{
    static class GuidList
    {
        public const string guidVSPackagePkgString = "eced64f5-d683-451b-8b50-821d19c7eb50";
        public const string guidVSPackageCmdSetString = "7c3dd686-d502-4019-8b44-67b2efb94304";
        
        public static readonly Guid guidVSPackageCmdSet = new Guid(guidVSPackageCmdSetString);
    };
}