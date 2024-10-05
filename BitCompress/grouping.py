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

    return seeker.best_comb

class GroupsSeeker():
    def __init__(self, gates_by_ports):
        self.gates_by_ports = gates_by_ports

        self.combs = {0: [CombinationGroups(self.gates_by_ports)]}
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

class CombinationGroups:
    def __init__(self, gates_by_ports, parent=None):
        self.parent = parent

        self.fixed = []
        self.gates_by_ports = gates_by_ports
        self.gates = []
        self.sub_combs = []

        self.gain = 0

    def discard(self):
        if self.parent is not None:
            self.parent.sub_combs.remove(self)

    def get_gain(self):
        return len(self.gates) - len(self.fixed)

    def fork(self):
        comb = CombinationGroups(self.gates_by_ports)
        comb.gates = self.gates
        comb.gain = self.gain

        self.sub_combs.append(comb)
        return comb

    def include_gates(self, port):
        for gate in self.gates_by_ports[port]:
            if gate not in self.gates:
                self.gates.append(gate)
                self.gain += 1

    def check_include(self, port):
        num_gates = 0
        for gate in self.gates_by_ports[port]:
            if gate not in self.gates:
                num_gates += 1

        return num_gates

    def check(self, port):
        include = False

        if len(self.fixed) == 0:
            include = True

        if include:
            self.fixed.append(port)
            self.include_gates(port)
