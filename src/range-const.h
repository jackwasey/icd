// Copyright (C) 2014 - 2016  Jack O. Wasey
//
// This file is part of icd.
//
// icd is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd. If not, see <http://www.gnu.org/licenses/>.

#include "icd_types.h"

// this is simplest just to hard-code
const CV vbase = CV::create("", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00");
const CV vbase_e = CV::create("", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00");
const CV v0 = CV::create("0", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09");
const CV v1 = CV::create("1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19");
const CV v2 = CV::create("2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29");
const CV v3 = CV::create("3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39");
const CV v4 = CV::create("4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49");
const CV v5 = CV::create("5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59");
const CV v6 = CV::create("6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69");
const CV v7 = CV::create("7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79");
const CV v8 = CV::create("8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89");
const CV v9 = CV::create("9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99");

const char* vbase_char[] = {"", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00"};
const char* vbase_e_char[] = {"", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00"};
const char* v0_char[] = {"0", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09"};
const char* v1_char[] = {"1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"};
const char* v2_char[] = {"2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29"};
const char* v3_char[] = {"3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39"};
const char* v4_char[] = {"4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49"};
const char* v5_char[] = {"5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59"};
const char* v6_char[] = {"6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69"};
const char* v7_char[] = {"7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79"};
const char* v8_char[] = {"8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89"};
const char* v9_char[] = {"9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"};

// horrible one-off code to pre-generate all the minor codes
inline CV MakeAllMinors() {
  CV vv = vbase;

  // create numbers 1 to 99 but cycle 10s first
  for (int i = 0; i < 10; ++i) {
    for (int j = 0; j < 10; ++j) {
      std::ostringstream s;
      s << j << i;
      if (i + j != 0)
        vv.push_back(s.str());
    }
  }
  return(vv);
}
// generate the lookups
const CV vv = MakeAllMinors();

const char* vv_char[] = {
  "", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "00",
  "0", "00", "01", "02", "03", "04", "05", "06", "07", "08", "09",
  "1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
  "2", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "3", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39",
  "4", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49",
  "5", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59",
  "6", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69",
  "7", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79",
  "8", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
  "9", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99"
};

const VecStr vv_std(vv_char, end(vv_char));
const VecStr v0_std(v0_char, end(v0_char));
const VecStr v1_std(v1_char, end(v1_char));
const VecStr v2_std(v2_char, end(v2_char));
const VecStr v3_std(v3_char, end(v3_char));
const VecStr v4_std(v4_char, end(v4_char));
const VecStr v5_std(v5_char, end(v5_char));
const VecStr v6_std(v6_char, end(v6_char));
const VecStr v7_std(v7_char, end(v7_char));
const VecStr v8_std(v8_char, end(v8_char));
const VecStr v9_std(v9_char, end(v9_char));
const VecStr vbase_e_std(vbase_e_char, end(vbase_e_char));

const VecStr v_empty_std(0L);
