#include "btf2llvm/src/lib.rs.h"
#include <vector>
#include <string>
#include <expected>

std::expected<std::vector<std::string>, std::string> GetSignaturesVector(const std::vector<std::string> &functions)
{
    try
    {
        ::rust::Vec<::rust::String> res = btf2llvm::get_signatures_vector(functions);
        std::vector<std::string> result{};
        result.reserve(res.size());
        for (const auto &s : res)
        {
            result.push_back((std::string) s);
        }
        return result;
    }
    catch (const std::exception &e)
    {
        return std::unexpected(e.what());
    }
    return std::unexpected("Something went wrong");
}


std::expected<std::string, std::string> GetSignaturesString(const std::vector<std::string> &functions)
{
    try
    {
        ::rust::String res = btf2llvm::get_signatures_string(functions);
        return (std::string) res;
    }
    catch (const std::exception &e)
    {
        return std::unexpected(e.what());
    }
    return std::unexpected("Something went wrong");
}
/*

std::expected<std::string, std::string> GetSignaturesAll() {
    try
    {
        ::rust::String res = btf2llvm::get_signatures_all();
        return (std::string) res;
    }
    catch (const std::exception &e)
    {
        return std::unexpected(e.what());
    }
    return std::unexpected("Something went wrong");
}*/


/*std::expected<std::vector<std::string>, std::string> GetSignatures(const std::vector<std::string> &functions)
{

    ::rust::Vec<::rust::String> res = btf2llvm::get_signatures(functions);
    // this method use an hack
    if (functions.size() != res.size()) {
        assert(functions.size() + 1 == res.size());
        std::string error = std::string(auto{res.back()});
        return std::unexpected(error);
    } else {
        std::vector<std::string> result;
        for (const auto &s : res)
        {
            std::string f = std::string(s);
            result.push_back(f);
        }
        return result;
    }
}
 */
