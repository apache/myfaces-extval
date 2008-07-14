/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.util;

import org.apache.myfaces.extensions.validator.core.WebXmlParameter;

import javax.el.ELContext;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import java.io.Externalizable;
import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author Gerhard Petracek
 */
public class FaceletsTaglibExpressionUtils {
    public static String tryToCreateValueBindingForFaceletsBinding(UIComponent uiComponent) {
        String faceletsValueBindingExpression = ELUtils.getValueBindingExpression(uiComponent);

        try {
            List<String> foundBindings = extractELTerms(uiComponent.getValueExpression("value"));

            return faceletsValueBindingExpression.substring(0, 1) + "{" + createBinding(foundBindings) + "}";
        } catch (Throwable t) {
            return faceletsValueBindingExpression;
        }
    }

    private static String createBinding(List<String> expressions) {
        String result = "";

        String prevFaceletsAttributeName = null;
        String currentBinding;
        String partOfBinding;

        int indexOfBindingDetails;
        String[] foundBindingDetails;
        String[] bindingDetails;

        for (String entry : expressions) {
            if (entry.startsWith("ValueExpression[")) {
                continue;
            }
            //TODO log entry

            foundBindingDetails = entry.split(" ");
            indexOfBindingDetails = findIndexOfBindingDetails(foundBindingDetails);
            if (indexOfBindingDetails == -1) {
                return null;
            }

            bindingDetails = foundBindingDetails[indexOfBindingDetails].split("=");

            if (bindingDetails.length < 2) {
                return null;
            }

            currentBinding = bindingDetails[1];
            if (prevFaceletsAttributeName != null) {
                partOfBinding = currentBinding.substring(currentBinding.indexOf(prevFaceletsAttributeName) + prevFaceletsAttributeName.length(), currentBinding.indexOf("}"));
                result = result + partOfBinding;
            } else {
                result = currentBinding.substring(currentBinding.indexOf("{") + 1, currentBinding.indexOf("}"));
            }

            prevFaceletsAttributeName = bindingDetails[0];
        }
        return result;
    }

    private static int findIndexOfBindingDetails(String[] bindingDetails) {
        int count = 0;
        for (String entry : bindingDetails) {
            if (entry.contains("=")) {
                return count;
            }
            count++;
        }
        return -1;
    }

    private static List<String> extractELTerms(Object o) {
        List<String> foundELTerms = new ArrayList<String>();
        try {
            if (resolveELTerms(o, new HashMap<Object, Object>(), foundELTerms, 0) > 0) {
                return foundELTerms;
            }
        }
        catch (Exception ex) {
            return null;
        }
        return null;
    }

    private static int resolveELTerms(Object o, Map<Object, Object> visited, List<String> foundELTerms, int count) throws Exception {
        if (o == null || visited.containsKey(o) || count > 50) {
            return 0;
        }

        visited.put(o, null);

        int elCount = 0;
        Class c = o.getClass();

        //inspect maps
        if (o instanceof java.util.Map) {

            for (Object entry : ((Map) o).values()) {
                elCount += resolveELTerms(entry, visited, foundELTerms, count + 1);
            }
            return elCount;
        }

        if (isELTerm(o)) {
            if (foundELTerms != null) {
                foundELTerms.add(o.toString());
            }
            return ++elCount;
        }

        //analyze arrays
        if (c.isArray()) {
            int length = Array.getLength(o);
            //check array [L -> no array of primitive types
            if (o.toString().startsWith("[L")) {
                for (int i = 0; i < length; i++) {
                    if (o.toString().startsWith("[Ljava.lang.String")) {
                        if (isELTerm(Array.get(o, i))) {
                            if (foundELTerms != null) {
                                foundELTerms.add(o.toString());
                            }
                            elCount++;
                        }
                    } else {
                        elCount += resolveELTerms(Array.get(o, i), visited, foundELTerms, count + 1);
                    }
                }
            }
            return elCount;
        }

        List<Field> attributes = findAllAttributes(c, new ArrayList<Field>());
        Field[] fields = (Field[]) attributes.toArray(new Field[attributes.size()]);

        AccessibleObject.setAccessible(fields, true);
        for (Field currentField : fields) {
            if (currentField.get(o) == null) {
                continue;
            }

            if (currentField.getType().equals(String.class)) {
                if (currentField.get(o) != null && isELTerm(currentField.get(o))) {
                    if (foundELTerms != null) {
                        foundELTerms.add(o.toString());
                    }
                    elCount++;
                }
            } else if (!currentField.getType().isPrimitive()) {
                elCount += resolveELTerms(currentField.get(o), visited, foundELTerms, count + 1);
            }
        }
        return elCount;
    }

    private static boolean isELTerm(Object o) {
        if (o instanceof ValueBinding || o instanceof Externalizable) {
            return false;
        }

        String s = o.toString();
        return ((s.contains("#") || s.contains("$")) && s.contains("{") && s.contains("}"));
    }

    private static List<Field> findAllAttributes(Class c, List<Field> attributes) {
        if (c == null) {
            return attributes;
        }
        findAllAttributes(c.getSuperclass(), attributes);

        Field[] fields = c.getDeclaredFields();
        for (Field currentField : fields) {
            if (!Modifier.isStatic(currentField.getModifiers())) {
                attributes.add(currentField);
            }
        }

        return attributes;
    }
}