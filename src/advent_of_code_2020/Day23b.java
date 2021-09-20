package advent_of_code_2020;

import java.util.*;

public class Day23b {
    static class CrabCups {
        Integer maxCup = Integer.MIN_VALUE;
        Integer minCup = Integer.MAX_VALUE;

        Map<Integer, Integer> ring = new HashMap<>();
        LinkedList<Integer> pickup = new LinkedList<>();
        Integer curr;

        CrabCups(int... labels) {
            curr = labels[0];
            for(int i = 0; i < labels.length; i++) {
                // put cup in ring
                ring.put(labels[i], labels[(i + 1) % labels.length]);

                // update min/max
                minCup = Math.min(minCup, labels[i]);
                maxCup = Math.max(maxCup, labels[i]);
            }

            // extra cups
            final int oneMillion = 1000000;

            // connect the last regular cup to the first extra cup
            int last = labels[labels.length - 1];
            ring.put(last, maxCup + 1);


            // put all but the last extra cup in the ring
            for (int i = maxCup + 1; i < oneMillion; i++) {
                ring.put(i, i + 1);
            }


            // add the last cup to the ring
            ring.put(oneMillion, labels[0]);
            maxCup = oneMillion;
        }

        void pickupOne() {
            // take cup to the right of current
            Integer cup = ring.get(curr);

            // remove it from the ring
            ring.put(curr, ring.get(cup));
            ring.remove(cup);

            // and put it in the pickup
            pickup.addFirst(cup);
        }

        void pickup() {
            pickupOne();
            pickupOne();
            pickupOne();
        }

        Integer findDest() {
            Integer dest = curr - 1;

            while (!ring.containsKey(dest)) {
                if (dest < minCup) {
                    dest = maxCup;
                } else {
                    dest--;
                }
            }

            return dest;
        }

        void putdownOne(Integer dest) {
            Integer cup = pickup.removeFirst();
            Integer next = ring.get(dest);

            ring.put(dest, cup);
            ring.put(cup, next);
        }

        void putdown() {
            Integer dest = findDest();
            putdownOne(dest);
            putdownOne(dest);
            putdownOne(dest);
        }

        void updateCurrent() {
            curr = ring.get(curr);
        }

        void step() {
            pickup();
            putdown();
            updateCurrent();
        }

        void doSteps(int count) {
            try {
                while(count > 0) {
                    step();
                    count--;
                }
            } catch (Exception ex) {
                System.out.println("ERROR");
                ex.printStackTrace();
                System.out.println("Steps left: " + count);
                System.out.println("Min " + minCup + ", Max " + maxCup);
                System.out.println("Curr " + curr);
            }
        }

        long part2() {
            int cup1 = ring.get(1);
            int cup2 = ring.get(cup1);

            return 1L * cup1 * cup2;
        }
    }

    public static void main(String[] args) {
        System.out.println("Start");
        long start = System.nanoTime();

        CrabCups cc = new CrabCups(6,5,3,4,2,7,9,1,8);
        cc.doSteps(10000000);
        System.out.println("Product: " + cc.part2());

        System.out.println("End, " + ((System.nanoTime() - start) / 1000000) + "ms");
    }
}
