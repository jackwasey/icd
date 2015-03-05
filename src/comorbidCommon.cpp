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

void lookupOneChunk(const VecVecInt& vcdb, const VecVecInt& map,
		const size_t num_comorbid, const size_t begin, const size_t end,
		Out& chunk) {

#ifdef ICD9_DEBUG_TRACE
	std::cout << "lookupComorbidChunk begin = " << begin << ", end = " << end << "\n";
#endif
	const Out falseComorbidChunk(num_comorbid * (1 + end - begin), false);
	chunk = falseComorbidChunk;
	// TODO: now parallel is working well, try looping through comorbidities in outside loop?
	for (size_t urow = begin; urow <= end; ++urow) { //end is index of end of chunk, so we include it in the loop.
#ifdef ICD9_DEBUG_TRACE
			std::cout << "lookupComorbidRangeOpenMP row: " << 1+urow-begin << " of " << 1+end-begin << "\n";
#endif
		for (VecVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) { // loop through icd codes for this visitId
#ifdef ICD9_DEBUG_TRACE
				std::cout << "cmb = " << cmb << "\n";
#endif
			for (int nve = 0; nve < 3; ++nve) { // N, V, then E codes... argh
#ifdef ICD9_DEBUG_TRACE
					std::cout << "nve = " << nve << ". vcdb_x length = " << vcdb.size() << "\n";
#endif

				const Codes& codes = vcdb[urow]; // these are the ICD-9 codes for the current visitid
				const Codes& mapCodes = map[cmb];

				const Codes::const_iterator cbegin = codes.begin();
				const Codes::const_iterator cend = codes.end();
				for (Codes::const_iterator code_it = cbegin; code_it != cend;
						++code_it) {
					bool found_it = std::binary_search(mapCodes.begin(),
							mapCodes.end(), *code_it);
					if (found_it) {
						const Out::size_type chunk_idx = num_comorbid
								* (urow - begin) + cmb;
#ifdef ICD9_DEBUG
						chunk.at(chunk_idx) = true;
#endif
#ifndef ICD9_DEBUG
						chunk[chunk_idx] = true;
#endif
						nve = 3;
						break;
					} // end found_it
				} // end loop through codes in one comorbidity
			} // end loop N, V and E codes
		} // end loop through all comorbidities
	} // end loop through visits
#ifdef ICD9_DEBUG_TRACE
	std::cout << "finished with one chunk\n";
#endif
}

void lookupComorbidByChunkFor(const VecVecInt& vcdb, const VecVecInt& map,
		const size_t chunkSize, const size_t ompChunkSize, Out& out) {
	const size_t num_comorbid = map.size();
	const size_t num_visits = vcdb.size();
	const size_t last_i = num_visits - 1;
	size_t chunk_end_i;
#ifdef ICD9_OPENMP
#pragma omp parallel shared(vcdb, map, out) private(chunk_end, chunk_out)
	omp_set_schedule(omp_sched_static, ompChunkSize); // ideally wouldn't repeat this over and over again
#ifdef ICD9_DEBUG_PARALLEL
	omp_sched_t* sched;
	int* threads;
	omp_get_schedule(sched, threads);
	std::out << "threads set = " << threads;
#endif
#pragma omp for ordered schedule(static)
#endif
	// loop through chunks at a time
	for (size_t vis_i = 0; vis_i < num_visits;) {
#ifdef ICD9_DEBUG_TRACE
		std::cout << "vis_i = " << vis_i << " ";
#endif
		chunk_end_i = vis_i + chunkSize - 1; // chunk end is an index, so for zero-based vis_i and chunk_end should be the last index in the chunk
		if (chunk_end_i > last_i)
			chunk_end_i = last_i; // indices
		Out chunk;
		lookupOneChunk(vcdb, map, num_comorbid, vis_i, chunk_end_i, chunk);
		//#ifdef ICD9_ORDER_GUARANTEE
#pragma omp ordered
		//#else
		//#pragma omp critical
		//#endif
		//	writeChunk(chunk_out, vis_i, out); // write the chunk (as critical not for coherence, but for false sharing)

		// (over)write a chunk in position to the full output matrix
		//		void writeChunk(const Out chunk_out, Out::size_type begin, Out& out) {
		{
#ifdef ICD9_DEBUG_TRACE
			std::cout << "writing a chunk beginning at: " << vis_i << "\n";
#endif
			std::copy(chunk.begin(), chunk.end(),
					out.begin() + (num_comorbid * vis_i));
		}
		vis_i += chunkSize;
	} // end parallel for
#ifdef ICD9_DEBUG
	std::cout << "finished looking up all chunks in for loop\n";
#endif
}

// just return the chunk results: this wouldn't cause invalidation of shared 'out'
Out lookupComorbidByChunkFor(const VecVecInt& vcdb, const VecVecInt& map,
		const int chunkSize, const int ompChunkSize) {
	Out out(vcdb.size() * map.size(), false);
	lookupComorbidByChunkFor(vcdb, map, chunkSize, ompChunkSize, out);
	return out;
}

