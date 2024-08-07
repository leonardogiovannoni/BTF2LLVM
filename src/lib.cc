#include "btf2llvm/src/lib.rs.h"
#include <iostream>
#include <vector>
#include <string>
#include <expected>

std::expected<std::vector<std::string>, std::string> GetSignatures(const std::vector<std::string> &functions)
{
    try
    {
        ::rust::Vec<::rust::String> res = btf2llvm::get_signatures(functions);
        std::vector<std::string> result;
        for (const auto &s : res)
        {
            std::string f = std::string(s);
            result.push_back(f);
        }
        return result;
    }
    catch (const std::exception &e)
    {
        return std::unexpected(e.what());
    }
}
