def sort_dict_by_length(dict):
    length = {}
    for k, v in dict.items():
        length[k] = len(v)

    return sorted(length.items(), key=lambda x: x[1])


def calculate_ports_groups(gates_by_ports):
    groups = {}
    ports_sorted = sort_dict_by_length(gates_by_ports)

    for port, times in ports_sorted:
        pass
    
    return groups