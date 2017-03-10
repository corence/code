
// all we want to know is:
// 1) given a set of towers and a set of enchantments, which is the best value?
// 2) if we apply it and then repeat the question, then?

import java.util.*;

public class Infinitode {
    public static class Enchantment {
        public final float price;
        public final Tower.Type towerType;
        public final Attribute effect;

        public Enchantment(float price, Tower.Type towerType, Attribute effect) {
            this.price = price;
            this.towerType = towerType;
            this.effect = effect;
        }

        public boolean canEnchant(Tower tower) {
            if(this.towerType != tower.type) return false;
            Attribute a = tower.attributes.get(this.effect.type);
            if(a == null) return false;
            if(a.level != this.effect.level - 1) return false;
            return true;
        }

        public Tower enchant(Tower tower) {
            if(!canEnchant(tower)) return tower;
            return tower.put(this.effect);
        }
    }

    public static class Attribute {
        public enum Type {
            Damage,
            APS
        }
        
        public final Type type;
        public final int level;
        public final float value;

        public Attribute(Type type, int level, float value) {
            this.type = type;
            this.level = level;
            this.value = value;
        }

        public String toString() {
            return "{" + type + " #" + level + ": " + value + "}";
        }
    }

    public static class Tower {
        public enum Type {
            Basic,
            Cannon
        }

        public final Type type;
        public final Map<Attribute.Type, Attribute> attributes;

        public Tower(Type type) {
            this.type = type;
            this.attributes = new HashMap<>();
        }

        public Tower(Type type, Map<Attribute.Type, Attribute> attributes) {
            this.type = type;
            this.attributes = new HashMap<>(attributes);
        }

        public Tower put(Attribute attribute) {
            Tower newTower = new Tower(this.type, this.attributes);
            newTower.attributes.put(attribute.type, attribute);
            return newTower;
        }

        public float valueIncrease(Enchantment enchantment) {
            return enchantment.enchant(this).dps() - this.dps();
        }

        public float dps() {
            return attributes.get(Attribute.Type.Damage).value * attributes.get(Attribute.Type.APS).value;
        }
    }

    public static class EnchantmentImpact {
        public final Tower tower;
        public final Enchantment enchantment;
        public final float value;

        public EnchantmentImpact(Tower tower, Enchantment enchantment, float value) {
            this.tower = tower;
            this.enchantment = enchantment;
            this.value = value;
        }

        public static EnchantmentImpact bestImpact(Collection<Tower> towers, Collection<Enchantment> enchantments) {
            EnchantmentImpact result = null;
            
            for(Tower t : towers) {
                for(Enchantment e : enchantments) {
                    float increase = t.valueIncrease(e);
                    if(result == null || increase > result.value) {
                        if(result == null) {
                            System.out.println("        " + "_ -> " + increase);
                        } else {
                            System.out.println("        " + result.value + " -> " + increase);
                        }
                        result = new EnchantmentImpact(t, e, t.valueIncrease(e));
                    }
                }
            }
            
            return result;
        }
    }

    public static void main(String[] args) {
        ArrayList<Tower> towers = generateTowers();
        ArrayList<Enchantment> enchantments = generateEnchantments();
        
        while(true) {
            EnchantmentImpact ei = EnchantmentImpact.bestImpact(towers, enchantments);
            if(ei == null || ei.value <= 0) {
                break;
            } else {
                System.out.println(
                        "Upgrade " +
                        ei.tower.type +
                        " " +
                        ei.tower.attributes.get(ei.enchantment.effect.type) +
                        " for " +
                        ei.enchantment.price +
                        " with " +
                        ei.enchantment.effect +
                        "; value increase: " +
                        ei.value);
                towers.remove(ei.tower);
                towers.add(ei.enchantment.enchant(ei.tower));
            }
        }
    }

    // test data
    public static ArrayList<Tower> generateTowers() {
        ArrayList<Tower> result = new ArrayList<>();
        
        Tower basic = new Tower(Tower.Type.Basic);
        basic = basic.put(new Attribute(Attribute.Type.APS, 0, 1.0f));
        basic = basic.put(new Attribute(Attribute.Type.Damage, -1, 0));
        result.add(basic);

        Tower cannon = new Tower(Tower.Type.Cannon);
        cannon = cannon.put(new Attribute(Attribute.Type.APS, 0, 0.3f));
        cannon = cannon.put(new Attribute(Attribute.Type.Damage, -1, 0));
        result.add(cannon);

        return result;
    }

    public static ArrayList<Enchantment> generateEnchantments() {
        ArrayList<Enchantment> result = new ArrayList<>();

        result.add(new Enchantment(40, Tower.Type.Basic, new Attribute(Attribute.Type.Damage, 0, 3.0f)));
        result.add(new Enchantment(10, Tower.Type.Basic, new Attribute(Attribute.Type.Damage, 1, 5.0f)));
        result.add(new Enchantment(8, Tower.Type.Basic, new Attribute(Attribute.Type.APS, 1, 1.2f)));
        
        result.add(new Enchantment(60, Tower.Type.Cannon, new Attribute(Attribute.Type.Damage, 0, 6.0f)));
        result.add(new Enchantment(20, Tower.Type.Cannon, new Attribute(Attribute.Type.Damage, 1, 12.0f)));
        result.add(new Enchantment(18, Tower.Type.Cannon, new Attribute(Attribute.Type.APS, 1, 0.5f)));

        return result;
    }
}
