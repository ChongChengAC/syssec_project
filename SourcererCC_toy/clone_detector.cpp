#include <iostream>
#include <vector>
#include <fstream>
#include <algorithm>
#include <unordered_map>
#include <cmath>
#include <set>

std::unordered_map<std::string, size_t> GTP;
std::vector<std::vector<std::string>> ordered_tokens;
std::vector<size_t> bag_size;
double threshold; 

void parse_file(unsigned file_id)
{
	std::string filename = "file" + std::to_string(file_id) + ".tokens";
	std::ifstream fin(filename);
	if (!fin)
		std::cerr << filename + ": No such file or directory!" <<
			std::endl;
	std::string line, token;
	size_t count;
	std::getline(fin, line);        // discard the first line
	std::vector<std::string> tokens_to_sort;
	while (fin >> token >> count) {
		GTP[token] += count;
		tokens_to_sort.emplace_back(token);
	}
	bag_size.push_back(tokens_to_sort.size());
	ordered_tokens.emplace_back(tokens_to_sort);
}

void sort_with_GTP()
{
	for (auto &file_tokens : ordered_tokens) {
		std::sort(file_tokens.begin(), file_tokens.end(),
			[](std::string a, std::string b)
			{ return GTP[a] < GTP[b]; } );
		for (const auto &p : file_tokens)
			std::cout << p << ' ' << GTP[p] << std::endl;
	}
}

void clone_detect(size_t x, size_t y)
{
	// |Bx| > |By|
	// max(|Bx|, |By|)
	unsigned max_size = bag_size[x];
	// 匹配θ * max(|Bx|, |By|)个token才算克隆
	unsigned least_common_tokens = std::ceil(threshold * max_size);
	if (bag_size[y] < least_common_tokens)
		return;
	
	// 子块大小为t-i+1
	unsigned subblock_size = max_size - least_common_tokens + 1;
	// 检测子块
	if (bag_size[y] > subblock_size) {
		std::set<std::string> sa(ordered_tokens[x].begin(),
			ordered_tokens[x].begin() + subblock_size);
		std::set<std::string> sb(ordered_tokens[y].begin(),
			ordered_tokens[y].begin() + subblock_size);
		std::set<std::string> common_tokens;
		std::set_intersection(sa.begin(), sa.end(), sb.begin(), 
			sb.end(), 
			std::inserter(common_tokens,common_tokens.begin()));
		// 子块无交集，不满足性质1
		if (common_tokens.empty())
			return;
		// 理论上限不足，不满足性质2
		if (common_tokens.size() + bag_size[y] - subblock_size <
						least_common_tokens)
			return;
	}
	// 通过性质1与性质2筛选后，对所有token进行比对
	// 在论文中，这一步实际上也有重要优化
        std::set<std::string> a(ordered_tokens[x].begin(),
                                ordered_tokens[x].end());
        std::set<std::string> b(ordered_tokens[y].begin(),
                                ordered_tokens[y].end());
        std::set<std::string> common_tokens;
        std::set_intersection(a.begin(), a.end(), b.begin(), b.end(),
                        std::inserter(common_tokens,
                        common_tokens.begin()));
        if (common_tokens.size() >= least_common_tokens)
                std::cout << "CLONE!" << std::endl;
}

int main()
{
	parse_file(1);
	parse_file(2);
	std::cout << "Please input theta (threshold) :" << std::endl;
	std::cin >> threshold;
	sort_with_GTP();
	if (bag_size[0] > bag_size[1])
		clone_detect(0, 1);
	else
		clone_detect(1, 0);
	return 0;
}

