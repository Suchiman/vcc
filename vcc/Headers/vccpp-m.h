//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------

#pragma once

#define updates(o) requires \wrapped(o) $ ensures \wrapped(o) $ writes o

#define maintains(o) requires o $ ensures o

#define always(claim, condition) requires \wrapped(claim) && \active_claim(claim) && \claims(claim, condition) $ ensures \wrapped(claim) && \active_claim(claim)
