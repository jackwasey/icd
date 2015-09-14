// Copyright (C) 2014 - 2015  Jack O. Wasey
//
// This file is part of icd9.
//
// icd9 is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// icd9 is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with icd9. If not, see <http://www.gnu.org/licenses/>.

// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::plugins(openmp)]]
#include <local.h>
#include <algorithm>
using namespace Rcpp;

void lookupOneChunk(const VecVecInt& vcdb, const VecVecInt& map,
		const VecVecIntSz num_comorbid, const VecVecIntSz begin,
		const VecVecIntSz end, ComorbidOut& chunk) {

#ifdef ICD9_DEBUG_TRACE
	Rcpp::Rcout << "lookupComorbidChunk begin = " << begin << ", end = " << end << "\n";
#endif
	const ComorbidOut falseComorbidChunk(num_comorbid * (1 + end - begin),
			false);
	chunk = falseComorbidChunk;
	for (VecVecIntSz urow = begin; urow <= end; ++urow) { //end is index of end of chunk, so we include it in the loop.
#ifdef ICD9_DEBUG_TRACE
			Rcpp::Rcout << "lookupComorbidRangeOpenMP row: " << 1+urow-begin << " of " << 1+end-begin << "\n";
#endif
		for (VecVecIntSz cmb = 0; cmb < num_comorbid; ++cmb) { // loop through icd codes for this visitId
#ifdef ICD9_DEBUG_TRACE
				Rcpp::Rcout << "cmb = " << cmb << "\n";
				Rcpp::Rcout << "vcdb_x length = " << vcdb.size() << "\n";
#endif

			const VecInt& codes = vcdb[urow]; // these are the ICD-9 codes for the current visitid
			const VecInt& mapCodes = map[cmb]; // may be zero length

			const VecInt::const_iterator cbegin = codes.begin();
			const VecInt::const_iterator cend = codes.end();
			for (VecInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				bool found_it = std::binary_search(mapCodes.begin(),
						mapCodes.end(), *code_it);
				if (found_it) {
					const ComorbidOut::size_type chunk_idx = num_comorbid
							* (urow - begin) + cmb;
#ifdef ICD9_DEBUG
					chunk.at(chunk_idx) = true;
#else
					chunk[chunk_idx] = true;
#endif
					break;
				} // end if found_it
			} // end loop through codes in one comorbidity
		} // end loop through all comorbidities
	} // end loop through visits
#ifdef ICD9_DEBUG_TRACE
	Rcpp::Rcout << "finished with one chunk\n";
#endif
}

void lookupComorbidByChunkFor(const VecVecInt& vcdb, const VecVecInt& map,
		const VecVecIntSz chunkSize, const VecVecIntSz ompChunkSize,
		ComorbidOut& out) {
	const VecVecIntSz num_comorbid = map.size();
	const VecVecIntSz num_visits = vcdb.size();
	const VecVecIntSz last_i = num_visits - 1;
	VecVecIntSz chunk_end_i;
	VecVecIntSz vis_i;
#ifdef ICD9_OPENMP
#pragma omp parallel default(none) shared(vcdb, map, out) private(chunk_end_i, vis_i)
	omp_set_schedule(omp_sched_static, ompChunkSize); // ideally wouldn't repeat this over and over again
#ifdef ICD9_DEBUG_PARALLEL
	omp_sched_t sched;
	int threads = 0;
	omp_get_schedule(&sched, &threads);
	Rcpp::Rcout << "threads per omp_get_schedule = " << threads << ". ";
	Rcpp::Rcout << "avail threads = " << omp_get_num_threads() << ". ";
	Rcpp::Rcout << "omp_get_thread_num = " << omp_get_thread_num() << "\n";
#endif

#pragma omp for schedule(static)
#endif
	// loop through chunks at a time
	for (vis_i = 0; vis_i < num_visits; vis_i += chunkSize) {
#ifdef ICD9_DEBUG_TRACE
		Rcpp::Rcout << "vis_i = " << vis_i << " ";
#endif
#if defined(ICD9_OPENMP) && defined(ICD9_DEBUG)
	  Rcpp::Rcout << "avail threads = " << omp_get_num_threads() << ". ";
		Rcpp::Rcout << "omp_get_thread_num = " << omp_get_thread_num() << "\n";
#endif

		chunk_end_i = vis_i + chunkSize - 1; // chunk end is an index, so for zero-based vis_i and chunk_end should be the last index in the chunk
		if (chunk_end_i > last_i)
			chunk_end_i = last_i; // indices
		ComorbidOut chunk;
		lookupOneChunk(vcdb, map, num_comorbid, vis_i, chunk_end_i, chunk);

	// next block doesn't need to be single threaded, but doing so improves cache contention
#ifdef ICD9_OPENMP
#pragma omp critical
#endif
		{
#ifdef ICD9_DEBUG_TRACE
			Rcpp::Rcout << "writing a chunk beginning at: " << vis_i << "\n";
#endif
			std::copy(chunk.begin(), chunk.end(),
					out.begin() + (num_comorbid * vis_i));
		}
		//vis_i += chunkSize;
	} // end parallel for
#ifdef ICD9_DEBUG
	Rcpp::Rcout << "finished looking up all chunks in for loop\n";
#endif
}

// just return the chunk results: this wouldn't cause invalidation of shared 'out'
ComorbidOut lookupComorbidByChunkFor(const VecVecInt& vcdb,
		const VecVecInt& map, const int chunkSize, const int ompChunkSize) {
	ComorbidOut out(vcdb.size() * map.size(), false);
	lookupComorbidByChunkFor(vcdb, map, chunkSize, ompChunkSize, out);
	return out;
}

