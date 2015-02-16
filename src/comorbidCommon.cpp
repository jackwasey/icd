// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
using namespace Rcpp;

void lookupComorbid(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits,
		const ComorbidVecInt::size_type num_comorbid, VecBool& out) {
	for (size_t urow = 0; urow < num_visits; ++urow) {
		// TODO any advantage in using iterator and distance?
		const VecUInt codes = allCodes[urow]; // these are the ICD-9 codes for the current visitid
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				if (std::binary_search(map[cmb].begin(), map[cmb].end(),
						*code_it)) {
					VecBool::size_type out_idx = num_comorbid * urow + cmb;
					out[out_idx] = true; // and update the current item. TODO break out to chunks to avoid contention.
				}
			}
		} // end for looping through whole comorbidity map
	}
}

// wrap lookupComorbid (by reference) to return data in new vector
VecBool lookupComorbid(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits, const ComorbidVecInt::size_type num_comorbid) {
	VecBool out;
	lookupComorbid(allCodes, map, num_visits, num_comorbid, out);
	return out;
}

VecBool lookupComorbidByRow(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits, const ComorbidVecInt::size_type num_comorbid) {
	VecBool out;
	lookupComorbidByRow(allCodes, map, num_visits, num_comorbid, out);
	return out;
}

void lookupComorbidByRow(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits, const ComorbidVecInt::size_type num_comorbid,
		VecBool& out) {
	for (size_t urow = 0; urow < num_visits; ++urow) {
		// TODO any advantage in using iterator and distance?
		const VecUInt codes = allCodes[urow]; // these are the ICD-9 codes for the current visitid
		VecBool row_out(num_comorbid, false);
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				if (std::binary_search(map[cmb].begin(), map[cmb].end(),
						*code_it)) {
					VecBool::size_type out_idx = num_comorbid * urow + cmb;
					//out[out_idx] = true;// and update the current item. TODO break out to chunks to avoid contention.
					row_out[cmb] = true; // and update the current item. TODO break out to chunks to avoid contention.
				}
			}
		} // end for looping through whole comorbidity map
		out.insert(out.end(), row_out.begin(), row_out.end());
	}
}

VecBool lookupComorbidByRowOpenMP(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits, const ComorbidVecInt::size_type num_comorbid) {
	VecBool out;
	lookupComorbidByRow(allCodes, map, num_visits, num_comorbid, out);
	return out;
}

void lookupComorbidByRowOpenMP(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits, const ComorbidVecInt::size_type num_comorbid,
		VecBool& out) {
#pragma omp parallel for schedule(static)
	for (size_t urow = 0; urow < num_visits; ++urow) {
		// TODO any advantage in using iterator and distance?
		const VecUInt codes = allCodes[urow]; // these are the ICD-9 codes for the current visitid
		VecBool row_out(num_comorbid, false);
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				if (std::binary_search(map[cmb].begin(), map[cmb].end(),
						*code_it)) {
					VecBool::size_type out_idx = num_comorbid * urow + cmb;
					//out[out_idx] = true;// and update the current item. TODO break out to chunks to avoid contention.
					row_out[cmb] = true; // and update the current item. TODO break out to chunks to avoid contention.
				}
			}
		} // end for looping through whole comorbidity map
		out.insert(out.end(), row_out.begin(), row_out.end());
	}
}

void lookupComorbidRangeOpenMP(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits, const ComorbidVecInt::size_type num_comorbid,
		std::size_t begin, std::size_t end, VecBool& out) {
	VecBool chunk_out(num_comorbid*(end-begin), false);
#pragma omp parallel for schedule(static)
	for (size_t urow = begin; urow < end; ++urow) {
		const VecUInt codes = allCodes[urow]; // these are the ICD-9 codes for the current visitid
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				if (std::binary_search(map[cmb].begin(), map[cmb].end(),
						*code_it)) {
					VecBool::size_type chunk_out_idx = num_comorbid*(urow-begin)+cmb;
					chunk_out[chunk_out_idx] = true; // and update the current item. TODO break out to chunks to avoid contention.
				}
			}
		} // end for looping through whole comorbidity map
	}
	// TODO: do I need to update a pre-allocate 'out'? Insert is certainly easier, but this might invalidate cache.
	out.insert(out.end(), chunk_out.begin(), chunk_out.end());
	writeChunk(chunk_out, begin, out);
}

VecBool lookupComorbidRangeOpenMP(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const MapVecInt::size_type num_visits, const ComorbidVecInt::size_type num_comorbid,
		std::size_t begin, std::size_t end) {
	VecBool out;
	lookupComorbidRangeOpenMP(allCodes, map, num_visits, num_comorbid, begin, end);
	return out;
}

/*
 * out as reference: assume it is pre-allocated
 */
void lookupComorbidByChunk(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, size_t chunkSize, VecBool& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type last_i = allCodes.size()-1;
	size_t vis_i=0;
	while (vis_i<last_i) {
		size_t chunk_end = vis_i + chunkSize;
		if (chunk_end>last_i) { chunk_end = last_i; }
		VecBool chunk_out = lookupComorbidRangeOpenMP(allCodes, map, last_i+1, num_comorbid, vis_i, chunk_end, out)
	}
}

VecBool lookupComorbidByChunk(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, size_t chunkSize) {
	VecBool out;
	lookupComorbidByChunk(allCodes, map, chunkSize);
	return out;
}

void writeChunk(const VecBool chunk_out, size_t begin, VecBool& out) {
	size_t end = chunk_out.size();
	for (size_t i = 0; i != end; ++i) {
		// TODO convert to [] for speed
		out.at(i+begin) = chunk_out(i);
	}
}
