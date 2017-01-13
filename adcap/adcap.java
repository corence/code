
/*
   - given:
       - profit per unit
       - price per unit
       - quantity had
       - quantity needed
 */

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.TreeMap;
import java.util.SortedMap;
import java.util.List;
import java.util.ArrayList;
import java.util.LinkedHashMap;

class Unlock {
    public final String name;
    public final String prerequisitePropertyName;
    public final BigInteger prerequisitePropertyCount;
    public final String targetPropertyName;
    public final BigDecimal targetPropertyMultiplier;

    public Unlock(String name, String prerequisitePropertyName, long prerequisitePropertyCount, String targetPropertyName, double targetPropertyMultiplier) {
        this.name = name;
        this.prerequisitePropertyName = prerequisitePropertyName;
        this.prerequisitePropertyCount = new BigInteger(Long.toString(prerequisitePropertyCount));
        this.targetPropertyName = targetPropertyName;
        this.targetPropertyMultiplier = new BigDecimal(targetPropertyMultiplier);
    }
}

class Property {
    public final BigInteger count;
    public final BigInteger basePrice;
    public final BigDecimal coefficient;

    public Property(BigInteger count, BigInteger basePrice, BigDecimal coefficient) {
        this.count = count;
        this.basePrice = basePrice;
        this.coefficient = coefficient;
    }
}

class State {
    public final List<Unlock> availableUnlocks = new ArrayList<Unlock>();
    public final LinkedHashMap<String, Property> properties;
}

class PurchaseOrder {
    public final String itemName;
    public final BigDecimal price;
    public final BigDecimal income_per_second;
}

class Methods {
    public final Unlock[] unlocks = {
        new Unlock("Free nitrogen", "Oxygen Bar", 1440, "Oxygen Bar", 55555)
    };
    
    public static SortedMap<BigDecimal, PurchaseOrder> purchaseOrders() {
        TreeMap<BigDecimal, PurchaseOrder> result = new TreeMap<BigDecimal, PurchaseOrder>();
    }
    
    public static SortedMap<BigDecimal, PurchaseOrder> purchaseOrdersFromUnlocks() {
        TreeMap<BigDecimal, PurchaseOrder> result = new TreeMap<BigDecimal, PurchaseOrder>();
    }
}
