def sort_dict_by_length(dict):
    length = {}
    for k, v in dict.items():
        length[k] = len(v)

    return sorted(length.items(), key=lambda x: x[1], reverse=True)


def calculate_ports_groups(gates_by_ports):
    #groups = {}
    ports_sorted = sort_dict_by_length(gates_by_ports)
    ports_by_length = {}
    for port, times in ports_sorted:
        if times not in ports_by_length:
            ports_by_length[times] = []
        ports_by_length[times].append(port)

    seeker = GroupsSeeker(gates_by_ports)

    for times, ports in ports_by_length.items():
        for port in ports:
            seeker.check(port)

    return seeker.best_combinations()

class GroupsSeeker():
    def __init__(self, gates_by_ports):
        self.gates_by_ports = gates_by_ports

        self.combs = {999994: [CombinationGroup(self.gates_by_ports)]}
        self.min_cycles = 999994
        self.best_comb = None

    def check(self, port):
        combs_items = self.combs.items()
        combs_items = sorted(combs_items, key=lambda x: x[0])

        max_checks = len(combs_items) // 2
        checks = 0
        for combs_gain, combinations in combs_items:
            for combination in combinations:
                comb = combination.fork()
                comb.check(port)

                gain = comb.get_gain()
                if gain < combs_gain and len(comb.ports) > 0:
                    if gain not in self.combs:
                        self.combs[gain] = []
                    self.combs[gain].append(comb)

                    if gain < self.min_cycles:
                        self.min_cycles = gain
                        self.best_comb = comb
                else:
                    comb.discard()

            checks += 1
            if checks > max_checks and False: # max checks
                break

    def best_combinations(self):
        combs_by_cyles = {}
        combs = []
        groups_hashes = []

        def check_comb(comb, max_cycles):
            cycles = comb.calc_gain()

            if cycles > max_cycles:
                return

            hash = comb.hash()
            if hash in groups_hashes:
                return

            groups_hashes.append(hash)

            if cycles not in combs_by_cyles:
                combs_by_cyles[cycles] = []

            if len(comb) > 0 and comb[0] != -1:
                combs_by_cyles[cycles].append(comb)
                combs.append(comb)

        combs_items = sorted(self.combs.items(), key=lambda x: x[0])
        for cycles, groups in combs_items:
            for group in groups:
                if len(group.gates) == 0:
                    continue

                basic = CombinationGroups()
                basic.add(group)

                for comb in combs:
                    fork = comb.fork()
                    fork.add(group)

                    check_comb(fork, cycles)

                check_comb(basic, cycles)

        combs_items = sorted(combs_by_cyles.items(), key=lambda x: x[0])

        if len(combs_items) > 0:
            groups = combs_items[0][1][0] # for the moment, take arbitrary the first element (of the last index)
            return groups.groups

        return []

class CombinationGroups: # a confusing name for a confusing file
    def __init__(self):
        self.groups = []
        self.gain = 0

    def hash(self):
        h = ''
        for group in self.groups:
            for port in group.ports:
                h += str(port)+':'
            h += '+'

        return h

    def add(self, group):
        if group in self.groups:
            return # debug repeat

        self.groups.append(group)

        if len(self.groups) > 1:
            self.gain = self.calc_gain()

    def fork(self):
        new_comb = CombinationGroups()
        new_comb.groups = self.groups
        new_comb.gain = self.gain
        return new_comb

    def calc_gain(self):
        gates = []
        hash_gate = {}
        gates_group = {}

        tot_ops = 0
        for group in self.groups:
            tot_ops += len(group.ports)
            for gate in group.gates:
                if gate not in gates:
                    gates.append(gate)

                ghash = gate.get_hash()

                if ghash not in gates_group:
                    gates_group[ghash] = []
                gates_group[ghash].append(group)

                hash_gate[ghash] = gate

        for ghash, groups in gates_group.items():
            gate = hash_gate[ghash]
            tot_ports = []
            for group in groups:
                for port in group.ports:
                    if port not in tot_ports:
                        tot_ports.append(port)

            gate_ports = gate.get_base_pins()
            tot_ops += (len(gate_ports)-len(tot_ports))

        return tot_ops

class CombinationGroup:
    def __init__(self, gates_by_ports, parent=None):
        self.parent = parent

        self.ports = []
        self.gates_by_ports = gates_by_ports
        self.gates = []
        self.sub_combs = []

    def discard(self):
        if self.parent is not None:
            self.parent.sub_combs.remove(self)

    def get_gain(self):
        num_ports = len(self.ports)
        needed_cycles = 0

        for gate in self.gates:
            ports = gate.get_base_pins()
            needed_cycles += len(ports) - num_ports

        return needed_cycles + num_ports

    def fork(self):
        comb = CombinationGroup(self.gates_by_ports)
        comb.gates = self.gates
        comb.ports = self.ports

        self.sub_combs.append(comb)
        return comb

    def add_port(self, port):
        if len(self.gates) == 0:
            self.gates = self.gates_by_ports[port]
        else:
            remove = []
            for gate in self.gates:
                if port not in gate.ports:
                    remove.append(gate)

    def check(self, port):
        self.add_port(port)
