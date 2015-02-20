// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
#ifdef _OPENMP // not available on clang
#include <omp.h>
#endif
using namespace Rcpp;

// (over)write a chunk in position to the full output matrix
void writeChunk(const Out chunk_out, Out::size_type begin, Out& out) {
#ifdef ICD9_DEBUG
	std::cout << "writing a chunk beginning at: " << begin << "\n";
#endif

	const Out::size_type chunk_size = chunk_out.size();
	for (Out::size_type i = 0; i != chunk_size; ++i) {
		Out::size_type j = i+begin;
#ifdef ICD9_DEBUG
		out.at(j) = chunk_out.at(i);
#endif
#ifndef ICD9_DEBUG
		out[j] = chunk_out[i];
#endif
	}
}

void lookupOneChunk(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const ComorbidVecInt::size_type num_comorbid, const std::size_t begin, const std::size_t end, Out& chunk_out) {
#ifdef ICD9_DEBUG
	std::cout << "lookupComorbidChunk begin = " << begin << ", end = " << end << "\n";
#endif
	const Out falseComorbidChunk(num_comorbid*(1+end-begin), false);
	chunk_out = falseComorbidChunk;
	for (size_t urow = begin; urow <= end; ++urow) { //end is an index, not a count
#ifdef ICD9_TRACE
		std::cout << "lookupComorbidRangeOpenMP row: " << urow-begin << " of " << end-begin << "\n";
#endif

		const VecUInt codes = allCodes[urow]; // these are the ICD-9 codes for the current visitid
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				bool found_it;
#ifdef ICD9_BINARY_SEARCH
				found_it = std::binary_search(map[cmb].begin(), map[cmb].end(), *code_it);
#endif
#ifdef ICD9_LINEAR_SEARCH
				found_it = std::find(map[cmb].begin(), map[cmb].end(), *code_it) != map[cmb].end();
#endif
				if (found_it) {
					const Out::size_type chunk_out_idx = num_comorbid*(urow-begin)+cmb;
#ifdef ICD9_DEBUG
					chunk_out.at(chunk_out_idx) = true;
#endif
#ifndef ICD9_DEBUG
					chunk_out[chunk_out_idx] = true;
#endif
				}
			} // end loop through one comorbidity
		} // end for looping through whole comorbidity map
	}
}

Out lookupOneChunk(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		ComorbidVecInt::size_type num_comorbid, const std::size_t begin, const std::size_t end) {
	Out chunk_out;
	lookupOneChunk(allCodes, map, num_comorbid, begin, end, chunk_out);
	return chunk_out;
}

void lookupComorbidByChunkFor(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const size_t chunkSize, const size_t ompChunkSize, Out& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const CodesVecSubtype::size_type num_visits = allCodes.size();
	const CodesVecSubtype::size_type last_i = num_visits-1;
	Out chunk_out;
	size_t chunk_end;
	// chunk size (in fact, chunkSize * num_comorbid must be word (maybe int?) length, so that bitwise vector bool is thread safe
#pragma omp parallel shared(allCodes, map, out) private(chunk_end, chunk_out)
#pragma omp for schedule(static,1)
	for (size_t vis_i=0; vis_i<last_i; vis_i+=chunkSize) {
		omp_set_schedule(omp_sched_static, ompChunkSize); // ideally wouldn't repeat this over and over again
#ifdef ICD9_DEBUG
		std::cout << "vis_i = " << vis_i << " ";
#endif
		chunk_end=vis_i+chunkSize;
		if (chunk_end>num_visits) { chunk_end=num_visits-1; }
		chunk_out = lookupOneChunk(allCodes, map, num_comorbid, vis_i, chunk_end);
#pragma omp critical
		writeChunk(chunk_out, vis_i, out); // write the chunk (as critical not for coherence, but for false sharing)
	}
}

// just return the chunk results: this wouldn't cause invalidation of shared 'out'
Out lookupComorbidByChunkFor(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const size_t chunkSize, const size_t ompChunkSize) {
	Out out(allCodes.size()*map.size(), false);
	lookupComorbidByChunkFor(allCodes, map, chunkSize, ompChunkSize, out);
	return out;
}

