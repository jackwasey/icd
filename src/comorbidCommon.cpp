// [[Rcpp::interfaces(r, cpp)]]
#include <Rcpp.h>
#include <local.h>
#include <string>
#include <algorithm>
#ifdef _OPENMP // not available on clang
#ifdef ICD9_OPENMP // only when I want to
#include <omp.h>
#endif
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

void lookupOneChunk(const CodesVecSubtype& vcdb_n, const CodesVecSubtype& vcdb_v, const CodesVecSubtype& vcdb_e,
		const ComorbidVecInt& map_n, const ComorbidVecInt& map_v, const ComorbidVecInt& map_e,
		const ComorbidVecInt::size_type num_comorbid, const std::size_t begin, const std::size_t end,
		Out& chunk_out) {
#ifdef ICD9_DEBUG
	std::cout << "lookupComorbidChunk begin = " << begin << ", end = " << end << "\n";
#endif
	const Out falseComorbidChunk(num_comorbid*(1+end-begin), false);
	chunk_out = falseComorbidChunk;
	// TODO: now parallel is working well, try looping through comorbidities in outside loop?
	for (size_t urow = begin; urow <= end; ++urow) { //end is an index, not a count
#ifdef ICD9_TRACE
		std::cout << "lookupComorbidRangeOpenMP row: " << urow-begin << " of " << end-begin << "\n";
#endif
		// TODO: do for V and E codes, too.
		const VecUInt codes = vcdb_n[urow]; // these are the ICD-9 codes for the current visitid
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				bool found_it;
#ifdef ICD9_BINARY_SEARCH
				found_it = std::binary_search(map_n[cmb].begin(), map_n[cmb].end(), *code_it);
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
#ifdef ICD9_DEBUG
		std::cout << "finished with one chunk\n";
#endif

}

Out lookupOneChunk(const CodesVecSubtype& vcdb_n, const CodesVecSubtype& vcdb_v, const CodesVecSubtype& vcdb_e,
		const ComorbidVecInt& map_n, const ComorbidVecInt& map_v, const ComorbidVecInt& map_e,
		ComorbidVecInt::size_type num_comorbid, const std::size_t begin, const std::size_t end) {
	Out chunk_out;
	lookupOneChunk(vcdb_n, vcdb_v, vcdb_e, map_n, map_v, map_e, num_comorbid, begin, end, chunk_out);
	return chunk_out;
}

void lookupComorbidByChunkFor(const CodesVecSubtype& vcdb_n, const CodesVecSubtype& vcdb_v, const CodesVecSubtype& vcdb_e,
		const ComorbidVecInt& map_n, const ComorbidVecInt& map_v, const ComorbidVecInt& map_e,
		const size_t chunkSize, const size_t ompChunkSize, Out& out) {
	const ComorbidVecInt::size_type num_comorbid = map_n.size();
	const CodesVecSubtype::size_type num_visits = vcdb_n.size();
	const CodesVecSubtype::size_type last_i = num_visits-1;
	Out chunk_out;
	size_t chunk_end;
	// chunk size (in fact, chunkSize * num_comorbid must be word (maybe int?) length, so that bitwise vector bool is thread safe
#ifdef ICD9_OPENMP
#pragma omp parallel shared(vcdb_n, vcdb_v, vcdb_e, map_n, map_v, map_e, out) private(chunk_end, chunk_out)
#pragma omp for schedule(static,4)
#endif
	for (size_t vis_i=0; vis_i<last_i; vis_i+=chunkSize) {
#ifdef ICD9_OPENMP
		omp_set_schedule(omp_sched_static, ompChunkSize); // ideally wouldn't repeat this over and over again
#endif
#ifdef ICD9_DEBUG
		std::cout << "vis_i = " << vis_i << " ";
#endif
		chunk_end=vis_i+chunkSize;
		if (chunk_end>num_visits) { chunk_end=num_visits-1; }
		chunk_out = lookupOneChunk(vcdb_n, vcdb_v, vcdb_e, map_n, map_v, map_e, num_comorbid, vis_i, chunk_end);
#ifdef ICD9_OPENMP
#pragma omp critical
#endif
		writeChunk(chunk_out, vis_i, out); // write the chunk (as critical not for coherence, but for false sharing)
	}
#ifdef ICD9_DEBUG
		std::cout << "finished looking up all chunks in for loop\n";
#endif
}

// just return the chunk results: this wouldn't cause invalidation of shared 'out'
Out lookupComorbidByChunkFor(const CodesVecSubtype& vcdb_n, const CodesVecSubtype& vcdb_v, const CodesVecSubtype& vcdb_e,
		const ComorbidVecInt& map_n, const ComorbidVecInt& map_v, const ComorbidVecInt& map_e,
		const size_t chunkSize, const size_t ompChunkSize) {
	Out out(vcdb_n.size()*map_n.size(), false);
	lookupComorbidByChunkFor(vcdb_n, vcdb_v, vcdb_e, map_n, map_v, map_e, chunkSize, ompChunkSize, out);
	return out;
}

