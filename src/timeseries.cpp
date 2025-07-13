#include <Rcpp.h>
#include <string>
#include <map>
#include <sstream>
#include <iomanip>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame preprocess_dates(CharacterVector dates, int forecast_months) {
  std::map<std::string, int> month_counts;
  int min_year = 9999, min_month = 12;
  int max_year = 0, max_month = 0;

  // Parses the date and finds the min/max year and month
  for (auto& date_str : dates) {
    std::string s = Rcpp::as<std::string>(date_str);
    if (s.length() < 7) continue;

    int year, month;
    if (sscanf(s.c_str(), "%d-%d", &year, &month) != 2) continue;

    // Update Minimum Date
    if (year < min_year || (year == min_year && month < min_month)) {
      min_year = year;
      min_month = month;
    }

    // Update Maximum Date
    if (year > max_year || (year == max_year && month > max_month)) {
      max_year = year;
      max_month = month;
    }

    // Create year and month keys
    std::ostringstream oss;
    oss << std::setw(4) << std::setfill('0') << year << "-"
        << std::setw(2) << std::setfill('0') << month;
    month_counts[oss.str()]++;
  }

  // Generate complete date series (including forecast months)
  std::vector<std::string> all_months;
  int current_year = min_year;
  int current_month = min_month;
  int total_months = (max_year - min_year) * 12 + (max_month - min_month) + 1 + forecast_months;

  for (int i = 0; i < total_months; ++i) {
    std::ostringstream oss;
    oss << std::setw(4) << std::setfill('0') << current_year << "-"
        << std::setw(2) << std::setfill('0') << current_month;
    all_months.push_back(oss.str());

    // Incremental month
    if (++current_month > 12) {
      current_month = 1;
      current_year++;
    }
  }

  // Creating a results dataframe
  CharacterVector ym_vec;
  IntegerVector count_vec;

  for (const auto& ym : all_months) {
    ym_vec.push_back(ym);
    count_vec.push_back(month_counts[ym]);
  }

  return DataFrame::create(
    Named("YearMonth") = ym_vec,
    Named("n") = count_vec
  );
}
