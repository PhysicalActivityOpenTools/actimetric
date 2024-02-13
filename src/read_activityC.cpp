#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]

NumericVector read_activityC(RawVector bytes, long double scale) {
              NumericVector res;

              int n = bytes.size() -1;
              bool alt = true;
              uint16_t data;

              for (int i=0; i < n; ++i) {
              if (alt) {
              data = (((bytes[i] << 8) | ((bytes[i+1]))) >> 4);
              } else {
              data = (((bytes[i] << 8) | ((bytes[i+1]))) & 0xFFF);
              i++;
              }
              alt = !alt;

              if (data > 0x7FF) data |= 0xF000;

              res.push_back(((int16_t)data) / scale * 1000.0f / 1000.0f);
              }

              return res;
}
