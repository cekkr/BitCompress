def sort_dict_by_length(dict):
    length = {}
    for k, v in dict.items():
        length[k] = len(v)

    return sorted(length.items(), key=lambda x: x[1], reverse=True)


def calculate_ports_groups(gates_by_ports):
    groups = {}
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

        self.combs = {0: [CombinationGroup(self.gates_by_ports)]}
        self.max_gain = 0
        self.best_comb = None

    def check(self, port):
        combs_items = self.combs.items()
        combs_items = sorted(combs_items, key=lambda x: x[0], reverse=True)

        max_checks = len(combs_items) // 2
        checks = 0
        for combs_gain, combinations in combs_items:

            for combination in combinations:
                comb = combination.fork()
                comb.check(port)

                gain = comb.get_gain()
                if gain > combs_gain:
                    if gain not in self.combs:
                        self.combs[gain] = []
                    self.combs[gain].append(comb)

                    if gain > self.max_gain:
                        self.max_gain = gain
                        self.best_comb = comb
                else:
                    comb.discard()

            checks += 1
            if checks > max_checks: # max checks
                break

    def best_combinations(self):
        combs = [self.best_comb]
        #todo
        return combs

class CombinationGroups: # a confusing name for a confusing file
    def __init__(self):
        self.groups = []

    def calc_gain(self):
        gates = []
        hash_gate = {}
        gate_repeat = {}
        gates_group = {}
        gain = 0
        for group in self.groups:
            gain += group.get_gain()
            for gate in group.gates:
                if gate not in gates:
                    gates.append(gate)

                ghash = gate.get_hash()

                if ghash not in gate_repeat:
                    gate_repeat[ghash] = 0
                gate_repeat[ghash] += 1

                if ghash not in gates_group:
                    gates_group[ghash] = []
                gates_group[ghash].append(group)

                hash_gate[ghash] = gate

        for ghash, groups in gates_group.items():
            gate = hash_gate[ghash]

            tot_ops = len(groups)
            tot_ports = []
            repeated = 0
            for group in groups:
                for port in group.ports:
                    if port not in tot_ports:
                        tot_ports.append(port)
                    else:
                        repeated += 1



        return gain

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

    def include_gates(self, port):

        if len(self.gates) == 0:
            self.gates = self.gates_by_ports[port]
        else:
            remove = []
            for gate in self.gates:
                if port not in gate.ports:
                    remove.append(gate)

    def check(self, port):
        include = False

        if len(self.ports) == 0:
            include = True

        if include:
            self.ports.append(port)
            self.include_gates(port)
