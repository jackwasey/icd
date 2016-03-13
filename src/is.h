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

#ifndef IS_H_
#define IS_H_
#include <string>
#include <vector>

bool icd9IsASingleV(const char* s);
bool icd9IsASingleE(const char* s);
bool icd9IsASingleVE(const char* s);
std::vector<bool> icd9_is_n_cpp(const std::vector<std::string>& sv);
std::vector<bool> icd9_is_v_cpp(const std::vector<std::string>& sv);
std::vector<bool> icd9_is_e_cpp(const std::vector<std::string>& sv);

#endif /* IS_H_ */
