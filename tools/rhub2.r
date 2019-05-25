# MAKEFLAGS="CXXFLAGS+=-std=c++14\ -Wno-ignored-attributes CXX11FLAGS+=-std=c++14\ -O3" GNUMAKEFLAGS="CXX11FLAGS+=-std=c++11\ -Wno-ignored-attributes" R CMD 
rhub::check_with_sanitizers(env_vars = c(GNUMAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes", MAKEFLAGS = "CXXFLAGS+=-std=c++11 \\ -Wno-ignored-attributes"))
rhub::check_with_sanitizers(env_vars = c(MAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes CXXFLAGS+=-std=c++11\\ -Wno-ignored-attributes"))
The following seems to work. I added -Wno-ignored-attributes just to reduce the massive Eigen compilation noise. 

```
Rscript -e 'rhub::check_with_sanitizers(env_vars = c(MAKEFLAGS = "CXX11FLAGS+=-std=c++11\\ -Wno-ignored-attributes CXXFLAGS+=-std=c++11\\ -Wno-ignored-attributes"))'
```

