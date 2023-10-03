class Params:

    def __init__(self, beta: float, gamma: float, import_rate: float,
                 hi_mu1=2.02, hi_mu2=2.69,
                 hi_sigma1=0.130, hi_sigma2=0.031,
                 hi_tau=0.039, hi_omega=0.79,
                 cop_alpha=2.844, cop_beta=1.299,
                 contact_matrix=None,
                 flight_contacts=None):
        """

        :type flight_contacts: object
        """
        if flight_contacts is None:
            flight_contacts = [0.19, 1]
        if contact_matrix is None:
            contact_matrix = [[1.0, 0.21], [0.21, 0.26]]

        self.beta = beta
        self.gamma = gamma
        self.import_rate = import_rate

        # HI antigen titre parameters
        self.hi_mu1 = hi_mu1
        self.hi_mu2 = hi_mu2
        self.hi_sigma1 = hi_sigma1
        self.hi_sigma2 = hi_sigma2
        self.hi_tau = hi_tau
        self.hi_omega = hi_omega

        # Correlate of protection from HI antigen titre parameters
        self.cop_alpha = cop_alpha
        self.cop_beta = cop_beta

        # Contact matrix defining contact rates between age-groups
        self.contact_matrix = contact_matrix
        self.flight_contacts = flight_contacts
