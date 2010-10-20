//-----------------------------------------------------------------------------
//
// Copyright (C) Microsoft Corporation.  All Rights Reserved.
//
//-----------------------------------------------------------------------------
using System;
using System.Collections.Generic;

namespace VccModel.Preprocessor
{
    public class ModelValuePrinter
    {

        private Dictionary<string, Dictionary<string, string>> renameMap = 
            new Dictionary<string, Dictionary<string, string>>();


        public void CreateOffsetValue(string typeName, string origValueName, bool isNegValue, UInt64 origValue, Int64 offset)
        {
            UInt64 value, posOffset;
            bool isNegOffset;
            string valueName, valueString;

            if (offset == 0) return;

            isNegOffset = (offset < 0);
            posOffset = (UInt64)((isNegOffset) ? -offset : offset);

            valueName = String.Format("{0} {1} {2}", origValueName, isNegOffset ? "-" : "+", posOffset);
            if (isNegValue)
            {
                value = (!isNegOffset) ? (origValue - posOffset) : (origValue + posOffset);
            }
            else
            {
                value = (isNegOffset) ? (origValue - posOffset) : (origValue + posOffset);
            }
            valueString = String.Format("{0}{1}", (isNegValue ? "-" : ""), value);
            if (!renameMap[typeName].ContainsKey(valueString))
            {
                renameMap[typeName][valueString] = valueName;
            }
        }

        public void CreateOffsetValues(string typeName, string valueName, bool isNegValue, UInt64 value, Int64 from, Int64 to)
        {
            Int64 i;
            if (from > to)
            {
                Int64 temp = from;
                from = to;
                to = temp;
            }

            for (i = from; i <= to; i++)
            {
                CreateOffsetValue(typeName, valueName, isNegValue, value, i);
            }
        }

        public void CreateOffsetValues(string typeName, string valueName, bool isNegValue, UInt64 value)
        {
            CreateOffsetValues(typeName, valueName, isNegValue, value, -4, 4);
        }

        public void CreatePowerOfTwo(string typeName, bool signed, int bits)
        {
            UInt64 value = 0;
            string valueName, valueString;
            int idx;

            for (idx = 9; idx < (bits - 1); idx++)
            {
                value = ((ulong)1 << idx);
                valueName = String.Format("2^{0}", idx);
                valueString = value.ToString();
                renameMap[typeName][valueString] = valueName;

                CreateOffsetValues(typeName, valueName, false, value);

                if (signed)
                {
                    valueString = "-" + valueString;
                    valueName = "-" + valueName;
                    renameMap[typeName][valueString] = valueName;
                    CreateOffsetValues(typeName, valueName, true, value);
                }
            }
        }

        public void CreateType(string typeName, string typeDescr, bool signed, int bits)
        {
            UInt64 value = 0;
            string valueName, valueString;

            if (!renameMap.ContainsKey(typeName))
            {
                renameMap[typeName] = new Dictionary<string, string>();

                if (signed)
                {
                    // Min value
                    value = ((ulong)1 << (bits - 1));
                    valueName = String.Format("{0}.MinValue", typeDescr);
                    valueString = String.Format("-{0}", value);
                    renameMap[typeName][valueString] = valueName;
                    CreateOffsetValues(typeName, valueName, true, value, 0, 4);
		    bits--;
                }

                // Max value
                value = ((ulong)1 << bits) - 1;
                valueName = String.Format("{0}.MaxValue", typeDescr);
                valueString = value.ToString();
                renameMap[typeName][valueString] = valueName;
                CreateOffsetValues(typeName, valueName, false, value, -4, 0);

                CreatePowerOfTwo(typeName, signed, bits);
            }
        }

        public ModelValuePrinter() {
            CreateType("^^i1", "Int8", true, 8);
            CreateType("^^u1", "UInt8", false, 8);
            CreateType("^^i2", "Int16", true, 16);
            CreateType("^^u2", "UInt16", false, 16);
            CreateType("^^i4", "Int32", true, 32);
            CreateType("^^u4", "UInt32", false, 32);
            CreateType("^^i8", "Int64", true, 64);
            CreateType("^^u8", "UInt64", false, 64);
        }

        public string Lookup(string typeName, string value)
        {
            if (renameMap.ContainsKey(typeName))
            {
                if (renameMap[typeName].ContainsKey(value)){
                    return String.Format("{0} ({1})", value, renameMap[typeName][value]);
                }
            }
            return value;
        }
    }
}
