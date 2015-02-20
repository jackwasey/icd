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

// insert a chunk at end of output matrix. THIS IS NOT THREAD SAFE.
void insertChunk(const Out& chunk_out, Out& out) {
#ifdef ICD9_TRACE
	std::ostringstream o;
	o << "inserting a chunk of size: " << chunk_out.size() << ", ";
	std::cout << o.str();
	std::cout.flush();
#endif
	out.insert(out.end(), chunk_out.begin(), chunk_out.end());
#ifdef ICD9_TRACE
	o << "out size now = " << out.size() << "\n";
	std::cout << o.str();
	std::cout.flush();

#endif
}

void lookupComorbidAllOneByOne(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, Out& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = allCodes.size();

	for (size_t urow = 0; urow < num_visits; ++urow) {
#ifdef ICD9_TRACE
		std::cout << "lookupComorbid row: " << urow << " of " << num_visits << "\n";
#endif

		const VecUInt codes = allCodes[urow]; // these are the ICD-9 codes for the current visitid
		for (ComorbidVecInt::size_type cmb = 0; cmb < num_comorbid; ++cmb) {
			// loop through icd codes for this visitId
			const VecUInt::const_iterator cbegin = codes.begin();
			const VecUInt::const_iterator cend = codes.end();
			for (VecUInt::const_iterator code_it = cbegin; code_it != cend;
					++code_it) {
				// #TODO consider switching search type according to size cutoff of map elements?
				bool found_it;
#ifdef ICD9_BINARY_SEARCH
				found_it = std::binary_search(map[cmb].begin(), map[cmb].end(), *code_it);
#endif
#ifdef ICD9_LINEAR_SEARCH
				found_it = std::find(map[cmb].begin(), map[cmb].end(), *code_it) != map[cmb].end();
#endif
				if (found_it) {
					const Out::size_type out_idx = num_comorbid * urow + cmb;
#ifdef ICD9_DEBUG
					out.at(out_idx) = true;
#endif
#ifndef ICD9_DEBUG
					out[out_idx] = true;
#endif
				}
			}
		} // end for looping through whole comorbidity map
	}
}

Out lookupComorbidAllOneByOne(const CodesVecSubtype& allCodes, const ComorbidVecInt& map) {
	Out out;
	lookupComorbidAllOneByOne(allCodes, map, out);
	return out;
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
				// #TODO consider switching search type according to size cutoff of map elements?
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

void lookupComorbidByChunkWhile(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, size_t chunkSize, Out& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = allCodes.size();
	const MapVecInt::size_type last_i = num_visits-1;
	size_t vis_i=0; // but see 19. in http://www.codeguru.com/cpp/cpp/cpp_mfc/general/article.php/c15419/32-OpenMP-Traps-for-C-Developers.htm#page-4;
	size_t chunk_end;
	Out chunk_out;
	// chunk size (in fact, chunkSize * num_comorbid must be word (maybe int?) length, so that bitwise vector bool is thread safe
#pragma omp parallel shared(allCodes, map, chunkSize, out) private(chunk_out, chunk_end, vis_i)
	while (vis_i<last_i) {
		//#pragma omp single
		//		{
		chunk_end = vis_i+chunkSize-1;
		if (chunk_end>last_i) { chunk_end=last_i; }
#ifdef ICD9_DEBUG
		std::cout << "working on chunk with rows: " << vis_i << " to " << chunk_end << "\n";
#endif
#pragma omp task
		// shared(allCodes, map, out) private (vis_i, chunk_end)
		chunk_out = lookupOneChunk(allCodes, map, num_comorbid, vis_i, chunk_end);
#pragma omp critical // deliberate bottleneck, should be less burden with bigger chunks and more threads
		writeChunk(chunk_out, vis_i, out);
#pragma omp atomic
		vis_i+=chunkSize;
#ifdef ICD9_DEBUG
#pragma omp critical // deliberate bottleneck, should be less burden with bigger chunks and more threads
		{
			std::cout << "vis_i now = " << vis_i << " ";
		}
#endif
	}
}

// just return the chunk results: this wouldn't cause invalidation of shared 'out'
Out lookupComorbidByChunkWhile(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, size_t chunkSize) {
	Out out(allCodes.size()*map.size(), false);
	lookupComorbidByChunkWhile(allCodes, map, chunkSize, out);
	return out;
}

void lookupComorbidByChunkFor(const CodesVecSubtype& allCodes, const ComorbidVecInt& map,
		const size_t chunkSize, const size_t ompChunkSize, Out& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = allCodes.size();
	const MapVecInt::size_type last_i = num_visits-1;
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

void lookupComorbidByRowFor(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, size_t chunkSize, Out& out) {
	const ComorbidVecInt::size_type num_comorbid = map.size();
	const MapVecInt::size_type num_visits = allCodes.size();
	const MapVecInt::size_type last_i = num_visits-1;
	Out chunk_out;
	// chunk size (in fact, chunkSize * num_comorbid must be word (maybe int?) length, so that bitwise vector bool is thread safe
#pragma omp parallel shared(allCodes, map, chunkSize, out) private(chunk_out)
#pragma omp for schedule(static,1)
	for (size_t vis_i=0; vis_i<last_i; ++vis_i) {
		omp_set_schedule(omp_sched_static, chunkSize);
#ifdef ICD9_DEBUG
		std::cout << "vis_i now = " << vis_i << " ";
		std::cout << "working on row: " << vis_i << "\n";
#endif
		chunk_out = lookupOneChunk(allCodes, map, num_comorbid, vis_i, vis_i);
		//#pragma omp critical
		writeChunk(chunk_out, vis_i, out); // write the ROW
	}
}

// just return the chunk results: this wouldn't cause invalidation of shared 'out'
Out lookupComorbidByRowFor(const CodesVecSubtype& allCodes, const ComorbidVecInt& map, size_t chunkSize) {
	Out out(allCodes.size()*map.size(), false);
	lookupComorbidByRowFor(allCodes, map, chunkSize, out);
	return out;
}


