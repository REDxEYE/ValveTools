from math import sqrt


class _Node(list):
    """
    simple wrapper around tree nodes - mainly to make the code a little more readable (although
    members are generally accessed via indices because its faster)
    """

    @property
    def point(self):
        return self[0]

    @property
    def left(self):
        return self[1]

    @property
    def right(self):
        return self[2]

    def is_leaf(self):
        return self[1] is None and self[2] is None


class ExactMatch(Exception):
    ...


class KdTree:
    """
    simple, fast python kd-tree implementation
    thanks to:
    http://en.wikipedia.org/wiki/Kd-tree
    """
    DIMENSION = 3  # dimension of points in the tree

    def __init__(self, data=()):
        self.root = None  # type:_Node
        self.perform_populate(data)

    def perform_populate(self, data):
        dimension = self.DIMENSION

        def populate_tree(points, depth):
            if not points:
                return None

            axis = depth % dimension

            # NOTE: this is slower than a DSU sort, but its a bit more readable,
            #  and the difference is only a few percent...
            points.sort(key=lambda point: point[axis])

            # find the half way point
            half = len(points) / 2

            node = _Node([points[half],
                          populate_tree(points[:half], depth + 1),
                          populate_tree(points[half + 1:], depth + 1)])

            return node

        self.root = populate_tree(data, 0)

    def get_closest(self, query_point, return_distances=False):
        """
        Returns the closest point in the tree to the given point

        NOTE: see the docs for get_within for info on the return_distances arg
        """
        dimension = self.DIMENSION

        dist_best = (self.root[0] - query_point).get_magnitude() ** 2
        best_list = [(dist_best, self.root[0])]

        def search(node, depth):
            node_point = node[0]

            axis = depth % dimension

            if query_point[axis] < node_point[axis]:
                near_node = node[1]
                far_node = node[2]
            else:
                near_node = node[2]
                far_node = node[1]

            # start the search
            if near_node is not None:
                search(near_node, depth + 1)

            # get the squared distance
            sd = 0
            for v1, v2 in zip(node_point, query_point):
                sd += (v1 - v2) ** 2

            cur_best = best_list[0][0]

            # if the point is closer than the currently stored one, insert it at the head
            if sd < cur_best:
                best_list.insert(0, (sd, node_point))

                # if its an exact match, bail
                if not sd:
                    raise ExactMatch
            else:
                best_list.append((sd, node_point))

            # check whether there could be any points on the other side of the
            # splitting plane that are closer to the query point than the current best
            if far_node is not None:
                if (node_point[axis] - query_point[axis]) ** 2 < cur_best:
                    search(far_node, depth + 1)

        try:
            search(self.root, 0)
        except ExactMatch:
            pass

        if return_distances:
            return best_list[0]

        return best_list[0][1]

    def get_within(self, query_point, threshold=1e-6, return_distances=False):
        """
        Returns all points that fall within the radius of the query_point within the tree.

        NOTE: if return_distances is True then the squared distances between the query_point and the points in the
        return list are returned.  This means the return list looks like this:
        [ (sqDistToPoint, point), ... ]

        This can be useful if you need to do more work on the results afterwards - just be aware that the distances
        in the list are squares of the actual distance between the points
        """
        dimension = self.DIMENSION
        ax_range_x, ax_range_y, ax_range_z = ((query_point[0] - threshold, query_point[0] + threshold),
                                              (query_point[1] - threshold, query_point[1] + threshold),
                                              (query_point[2] - threshold, query_point[2] + threshold))

        sq_threshold = threshold ** 2

        matches = []

        def search(node, depth):
            node_point = node[0]

            axis = depth % dimension

            if query_point[axis] < node_point[axis]:
                near_node = node[1]
                far_node = node[2]
            else:
                near_node = node[2]
                far_node = node[1]

            # start the search
            if near_node is not None:
                search(near_node, depth + 1)

            # test this point
            if ax_range_x[0] <= node_point[0] <= ax_range_x[1]:
                if ax_range_y[0] <= node_point[1] <= ax_range_y[1]:
                    if ax_range_z[0] <= node_point[2] <= ax_range_z[1]:
                        sd = 0
                        for v1, v2 in zip(node_point, query_point):
                            sd += (v1 - v2) ** 2

                        if sd <= sq_threshold:
                            matches.append((sd, node_point))

            if far_node is not None:
                if (node_point[axis] - query_point[axis]) ** 2 < sq_threshold:
                    search(far_node, depth + 1)

        search(self.root, 0)
        # the best is guaranteed to be at the head of the list,
        #  but consequent points might be out of order - so order them now
        matches.sort()

        if return_distances:
            return matches

        return [m[1] for m in matches]

    def get_distance_ratio_weighted_vector(self, query_point, ratio=2, return_distances=False):
        """
        Finds the closest point to the query_point in the tree and returns all points within a distance
        of ratio*<closest point distance>.

        This is generally more useful that using get_within because get_within could return an exact
        match along with a bunch of points at the outer search limit and thus heavily bias the
        results.

        NOTE: see docs for get_within for details on the returnDistance arg
        """
        assert ratio > 1
        closest_dist, closest = self.get_closest(query_point, return_distances=True)
        if closest_dist == 0:
            if return_distances:
                return [(0, closest)]

            return [closest]

        closest_dist = sqrt(closest_dist)
        max_dist = closest_dist * ratio

        return self.get_within(query_point, max_dist, return_distances=return_distances)

# end
