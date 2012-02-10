//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#pragma once

#define updates(o) requires \wrapped(o) | ensures \wrapped(o) | writes o
