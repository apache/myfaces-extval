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
package org.apache.myfaces.extensions.validator.core.el;

import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Iterator;

/**
 * Helper class to get the real/full value binding - tested with facelets 1.1.14
 * The target is to get rid of this impl. - currently it's a workaround to support custom facelets components.
 * An alternative would be an EL-Resolver - there are still some open issues with such an approach
 * + It isn't available with JSF 1.1.x
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
class FaceletsTaglibExpressionHelper
{
    public static ValueBindingExpression tryToCreateValueBindingForFaceletsBinding(UIComponent uiComponent)
    {
        String faceletsValueBindingExpression = DefaultELHelper.getOriginalValueBindingExpression(uiComponent);

        try
        {
            List<String> foundBindings = extractELTerms(
                ExtValUtils.getELHelper().getBindingOfComponent(uiComponent, "value"));

            Map<String, String> mappedFaceletsVars = new HashMap<String, String>();
            ValueBindingExpression vbe= new ValueBindingExpression(faceletsValueBindingExpression
                    .substring(0, 1) + "{" + createBinding(foundBindings, mappedFaceletsVars) + "}");

            Class entityClass = ExtValUtils.getELHelper()
                .getTypeOfExpression(FacesContext.getCurrentInstance(), vbe.getBaseExpression());

            if(entityClass == null)
            {
                return tryToReplaceVars(vbe, mappedFaceletsVars);
            }
            return vbe;
        }
        catch (Throwable t)
        {
            return new ValueBindingExpression(faceletsValueBindingExpression);
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    private static String createBinding(List<String> expressions, Map<String, String> virtualVars)
    {
        String currentBinding;

        int indexOfBindingDetails;
        String[] foundBindingDetails;
        String[] bindingDetails;

        Map<String, String> addedVirtualNames = new HashMap<String, String>();

        for (String entry : expressions)
        {
            if (entry.startsWith("ValueExpression["))
            {
                continue;
            }

            foundBindingDetails = entry.split(" ");
            indexOfBindingDetails = findIndexOfBindingDetails(foundBindingDetails);

            if (indexOfBindingDetails == -1)
            {
                return null;
            }

            bindingDetails = foundBindingDetails[indexOfBindingDetails].split("=");

            if (bindingDetails.length < 2)
            {
                return null;
            }

            currentBinding = bindingDetails[1];

            //to support blanks within a binding with map syntax
            if(currentBinding.contains("{") && !currentBinding.contains("}"))
            {
                currentBinding = addFurtherBindingParts(currentBinding, foundBindingDetails, indexOfBindingDetails);
            }

            if (currentBinding.contains("}"))
            {
                //entry for "virtual" facelets beans
                if(!addedVirtualNames.containsKey(bindingDetails[0]))
                {
                    addedVirtualNames.put(bindingDetails[0], currentBinding);
                }
            }
            //entry for "virtual" facelets var
            if(!(currentBinding.contains("{") || currentBinding.contains("}")))
            {
                virtualVars.put(bindingDetails[0], bindingDetails[1].substring(1, bindingDetails[1].length()-2));
            }
        }

        String originalBinding = addedVirtualNames.get("value");
        originalBinding = originalBinding.substring(originalBinding.indexOf("{") + 1, originalBinding.indexOf("}"));
        addedVirtualNames.remove("value");
        return tryToTransformToRealBinding(originalBinding, addedVirtualNames);
    }

    private static String tryToTransformToRealBinding(String originalBinding, Map<String, String> addedVirtualNames)
    {
        originalBinding = "#{" + originalBinding + "}";
        Iterator nameIterator = addedVirtualNames.keySet().iterator();

        String currentKey;
        String currentValue;
        while(nameIterator.hasNext())
        {
            currentKey = (String) nameIterator.next();
            currentValue = addedVirtualNames.get(currentKey);

            currentValue = currentValue.substring(currentValue.indexOf("{") + 1, currentValue.indexOf("}"));
            originalBinding = originalBinding.replace("{" + currentKey + ".", "{" + currentValue + ".");
            originalBinding = originalBinding.replace("." + currentKey + ".", "." + currentValue + ".");
            originalBinding = originalBinding.replace("[" + currentKey + "]", "[" + currentValue + "]");
        }

        return originalBinding.substring(2, originalBinding.length() - 1);
    }

    //to support blanks - e.g. with map syntax
    private static String addFurtherBindingParts(String currentBinding, String[] foundBindingDetails,
                                                 int indexOfBindingDetails)
    {
        for(int i = indexOfBindingDetails + 1; i < foundBindingDetails.length; i++)
        {
            currentBinding += foundBindingDetails[i];
            if(foundBindingDetails[i].contains("}"))
            {
                return currentBinding;
            }
        }
        return currentBinding;
    }

    private static int findIndexOfBindingDetails(String[] bindingDetails)
    {
        int count = 0;
        for (String entry : bindingDetails)
        {
            if (entry.contains("="))
            {
                return count;
            }
            count++;
        }
        return -1;
    }

    private static List<String> extractELTerms(Object o)
    {
        List<String> foundELTerms = new ArrayList<String>();
        try
        {
            if (resolveELTerms(o, new HashMap<Object, Object>(), foundELTerms, 0) > 0)
            {
                return foundELTerms;
            }
        }
        catch (Exception ex)
        {
            return null;
        }
        return null;
    }

    private static int resolveELTerms(Object o, Map<Object, Object> visited,
                                      List<String> foundELTerms, int count) throws Exception
    {
        if (o == null || visited.containsKey(o) || count > 50)
        {
            return 0;
        }

        visited.put(o, null);

        int elCount = 0;
        Class c = o.getClass();

        //inspect maps
        if (o instanceof Map)
        {

            for (Object entry : ((Map) o).values())
            {
                //found entry for "virtual" facelets var
                if(entry.toString().contains("ValueExpression["))
                {
                    foundELTerms.add(entry.toString());
                }
                elCount += resolveELTerms(entry, visited, foundELTerms, count + 1);
            }
            return elCount;
        }

        if (ExtValUtils.getELHelper().isELTermWellFormed(o))
        {
            if (foundELTerms != null)
            {
                foundELTerms.add(o.toString());
            }
            return ++elCount;
        }

        //analyze arrays
        if (c.isArray())
        {
            int length = Array.getLength(o);
            //check array [L -> no array of primitive types
            if (o.toString().startsWith("[L"))
            {
                for (int i = 0; i < length; i++)
                {
                    if (o.toString().startsWith("[Ljava.lang.String"))
                    {
                        if (ExtValUtils.getELHelper().isELTermWellFormed(Array.get(o, i)))
                        {
                            if (foundELTerms != null)
                            {
                                foundELTerms.add(o.toString());
                            }
                            elCount++;
                        }
                    }
                    else
                    {
                        elCount += resolveELTerms(Array.get(o, i), visited, foundELTerms, count + 1);
                    }
                }
            }
            return elCount;
        }

        List<Field> attributes = findAllAttributes(c, new ArrayList<Field>());
        Field[] fields = attributes.toArray(new Field[attributes.size()]);

        AccessibleObject.setAccessible(fields, true);
        for (Field currentField : fields)
        {
            if (currentField.get(o) == null)
            {
                continue;
            }

            if (currentField.getType().equals(String.class))
            {
                if (currentField.get(o) != null && ExtValUtils.getELHelper().isELTermWellFormed(currentField.get(o)))
                {
                    if (foundELTerms != null)
                    {
                        foundELTerms.add(o.toString());
                    }
                    elCount++;
                }
            }
            else if (!currentField.getType().isPrimitive())
            {
                elCount += resolveELTerms(currentField.get(o), visited, foundELTerms, count + 1);
            }
        }
        return elCount;
    }

    private static List<Field> findAllAttributes(Class c, List<Field> attributes)
    {
        if (c == null)
        {
            return attributes;
        }
        findAllAttributes(c.getSuperclass(), attributes);

        Field[] fields = c.getDeclaredFields();
        for (Field currentField : fields)
        {
            if (!Modifier.isStatic(currentField.getModifiers()))
            {
                attributes.add(currentField);
            }
        }

        return attributes;
    }

    private static ValueBindingExpression tryToReplaceVars(ValueBindingExpression valueBindingExpression,
                                                            Map<String, String> mappedFaceletsVars)
    {
        String property;
        String result = "";
        boolean last = false;

        while(true)
        {
            if(valueBindingExpression.getBaseExpression() == null)
            {
                last = true;
            }


            property = valueBindingExpression.getProperty();

            valueBindingExpression = ValueBindingExpression
                .replaceProperty(valueBindingExpression, getNewProperty(property, mappedFaceletsVars));

            if(result.length() == 0)
            {
                result = valueBindingExpression.getProperty();
            }
            else
            {
                result = valueBindingExpression.getProperty() + "." + result;
            }

            valueBindingExpression = valueBindingExpression.getBaseExpression();

            if(last)
            {
                break;
            }
        }
        return new ValueBindingExpression(valueBindingExpression.getPrefix() + "{" + result + "}");
    }

    private static String getNewProperty(String oldProperty, Map<String, String> mappedFaceletsVars)
    {
        List<String> virtualVars = getPotentialVirtualVars(oldProperty);

        for(String virtualVar : virtualVars)
        {
            if(mappedFaceletsVars.containsKey(virtualVar))
            {
                oldProperty = replacePropertyValue(oldProperty, virtualVar, mappedFaceletsVars.get(virtualVar));
            }
        }
        return oldProperty;
    }

    private static List<String> getPotentialVirtualVars(String oldProperty)
    {
        int start = -1;
        int end = -1;

        List<String> virtualVarList = new ArrayList<String>();

        for(int i = 0; i < oldProperty.length(); i++)
        {
            if(start == - 1 && oldProperty.charAt(i) == '[')
            {
                start = i + 1;
            }
            else if((start != - 1 && oldProperty.charAt(i) == '[') || oldProperty.charAt(i) == ']')
            {
                end = i;
            }

            if(start != -1 && end != -1)
            {
                virtualVarList.add(oldProperty.substring(start, end));
                if(oldProperty.charAt(i) == '[')
                {
                    start = i + 1;
                }
                else
                {
                    start = -1;
                }
                end = -1;
            }
        }

        return virtualVarList;
    }

    private static String replacePropertyValue(String oldProperty, String targetVar, String newValue)
    {
        int index = oldProperty.indexOf(targetVar);

        if(index == -1)
        {
            return oldProperty;
        }

        String result = oldProperty.substring(0, index);
        result += newValue;
        return result + oldProperty.substring(index + targetVar.length(), oldProperty.length());
    }

    /*
     * replace virtual facelets vars (map syntax)
     * tested styles (simple and nested): test[ix[ix2[ix3]]]
     */
    /*
    private static String _createBinding(List<String> expressions, Map<String, String> virtualVars)
    {
        String result = "";

        String prevFaceletsAttributeName = null;
        String currentBinding;
        String partOfBinding;

        int indexOfBindingDetails;
        String[] foundBindingDetails;
        String[] bindingDetails;

        List<String> addedVirtualNames = new ArrayList<String>();

        for (String entry : expressions)
        {
            if (entry.startsWith("ValueExpression["))
            {
                continue;
            }

            foundBindingDetails = entry.split(" ");
            indexOfBindingDetails = findIndexOfBindingDetails(foundBindingDetails);

            if (indexOfBindingDetails == -1)
            {
                return null;
            }

            bindingDetails = foundBindingDetails[indexOfBindingDetails].split("=");

            if (bindingDetails.length < 2)
            {
                return null;
            }

            currentBinding = bindingDetails[1];

            //to support blanks within a binding with map syntax
            if(currentBinding.contains("{") && !currentBinding.contains("}"))
            {
                currentBinding = addFurtherBindingParts(currentBinding, foundBindingDetails, indexOfBindingDetails);
            }

            if (prevFaceletsAttributeName != null && currentBinding.contains("}"))
            {
                //entry for "virtual" facelets beans
                if(!addedVirtualNames.contains(bindingDetails[0]))
                {
                    partOfBinding = currentBinding.substring(currentBinding.indexOf(prevFaceletsAttributeName)
                        + prevFaceletsAttributeName.length(), currentBinding.indexOf("}"));

                    addedVirtualNames.add(bindingDetails[0]);
                    result = result + partOfBinding;
                }
                else
                {
                    continue;
                }
            }
            //entry for "virtual" facelets var
            else if(!(currentBinding.contains("{") || currentBinding.contains("}")))
            {
                virtualVars.put(bindingDetails[0], bindingDetails[1].substring(1, bindingDetails[1].length()-2));
                continue;
            }
            else
            {
                if(!addedVirtualNames.contains(bindingDetails[0]))
                {
                    addedVirtualNames.add(bindingDetails[0]);
                    result = currentBinding.substring(currentBinding.indexOf("{") + 1, currentBinding.indexOf("}"));
                }
            }

            prevFaceletsAttributeName = bindingDetails[0];
        }
        return result;
    }
    */
}