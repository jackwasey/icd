// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
using namespace Rcpp;

// (over)write a chunk in position to the full output matrix
void writeChunk(const VecBool chunk_out, VecBool::size_type begin, VecBool& out) {
	VecBool::size_type chunk_size = chunk_out.size();
	for (VecBool::size_type i = 0; i != chunk_size; ++i) {
		VecBool::size_type j = i+begin;
		out.at(j) = chunk_out.at(i);		// TODO convert to [] for speed
	}
}

// insert a chunk at end of output matrix
void insertChunk(const VecBool chunk_out, VecBool& out) {
	out.insert(out.end(), chunk_out.begin(), chunk_out.end());
}

void lookupComorbid(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, VecBool& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = allCodes.size();

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
					//out[out_idx] = true; // and update the current item.
					out.at(out_idx) = true; // TODO: convert back to [] access for speed
				}
			}
		} // end for looping through whole comorbidity map
	}
}

// wrap lookupComorbid (by reference) to return data in new vector
VecBool lookupComorbid(const CodesVecSubtype& allCodes, const ComorbidVecInt& map) {
	VecBool out;
	lookupComorbid(allCodes, map, out);
	return out;
}

void lookupComorbidByRow(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, VecBool& out) {
	lookupComorbidByChunk(allCodes, map, 1, out);
}

void lookupComorbidRangeOpenMP(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		ComorbidVecInt::size_type num_comorbid,
		std::size_t begin, std::size_t end, VecBool& out) {
	// TODO; this is the one place NOT to recalc sizes

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
	insertChunk(chunk_out, out);
}

void lookupComorbidByChunk(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, size_t chunkSize, VecBool& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = allCodes.size();
	const MapVecInt::size_type last_i = num_visits-1;
	size_t vis_i=0;
	VecBool chunk_out;
	while (vis_i<last_i) {
		size_t chunk_end = vis_i + chunkSize;
		if (chunk_end>last_i) { chunk_end = last_i; }
		lookupComorbidRangeOpenMP(allCodes, map, num_comorbid, vis_i, chunk_end, out);
		insertChunk(chunk_out, out);
	}
}

// just return the chunk results: this wouldn't cause invalidation of shared 'out'
VecBool lookupComorbidByChunk(const CodesVecSubtype& allCodes,
		const ComorbidVecInt& map, size_t chunkSize) {
	VecBool out;
	lookupComorbidByChunk(allCodes, map, chunkSize, out);
	return out;
}


