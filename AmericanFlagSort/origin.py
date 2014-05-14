def get_radix_val(x, digit, radix):
    return int(floor(x / radix**digit)) % radix

def compute_offsets(a_list, start, end, digit, radix):
    counts = [0 for _ in range(radix)]
    for i in range(start, end):
        val = get_radix_val(a_list[i], digit, radix)
        counts[val] += 1
    offsets = [0 for _ in range(radix)]
    sum = 0
    for i in range(radix):
        offsets[i] = sum
        sum += counts[i]
    return offsets

def swap(a_list, offsets, start, end, digit, radix):
    i = start
    next_free = copy(offsets)
    cur_block = 0
    while cur_block < radix-1:
        if i >= offsets[cur_block+1]:
            cur_block += 1
            continue
        radix_val = get_radix_val(a_list[i], digit, radix)
        if radix_val == cur_block:
            i += 1
            continue
        swap_to = next_free[radix_val]
        a_list[i], a_list[swap_to] = a_list[swap_to], a_list[i]
        next_free[radix_val] += 1

def american_flag_sort_helper(a_list, start, end, digit, radix):
    offsets = compute_offsets(a_list, start, end, digit, radix)
    swap(a_list, offsets, start, end, digit, radix)
    if digit == 0:
        return
    for i in range(len(offsets)-1):
        american_flag_sort_helper(a_list, offsets[i], offsets[i+1], digit-1, radix)

def american_flag_sort(a_list, radix):
    for x in a_list:
        assert(type(x) == int)
    max_val = max(a_list)
    max_digit = int(floor(log(max_val, radix)))
    american_flag_sort_helper(a_list, 0, len(a_list), max_digit, radix)
