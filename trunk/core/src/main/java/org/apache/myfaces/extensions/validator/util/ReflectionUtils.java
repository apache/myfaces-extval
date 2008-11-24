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

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.StringTokenizer;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ReflectionUtils
{
    public static Method tryToGetMethod(Class targetClass, String targetMethodName)
    {
        return tryToGetMethod(targetClass, targetMethodName, null);
    }

    public static Method tryToGetMethod(Class targetClass, String targetMethodName, Class... parameterTypes)
    {
        try
        {
            return getMethod(targetClass, targetMethodName, parameterTypes);
        }
        catch (Throwable t)
        {
            //do nothing - it's just a try
            return null;
        }
    }

    public static Method getMethod(Class targetClass, String targetMethodName)
        throws NoSuchMethodException
    {
        return getMethod(targetClass, targetMethodName, null);
    }

    public static Method getMethod(Class targetClass, String targetMethodName, Class... parameterTypes)
        throws NoSuchMethodException
    {
        Class currentClass = targetClass;
        Method targetMethod = null;
        
        while (!Object.class.getName().equals(currentClass.getName()))
        {
            try
            {
                targetMethod = currentClass.getDeclaredMethod(targetMethodName, parameterTypes);
                break;
            }
            catch (NoSuchMethodException e)
            {
                currentClass = currentClass.getSuperclass();
            }
        }

        if(targetMethod == null)
        {
            for (Class currentInterface : targetClass.getInterfaces())
            {
                currentClass = currentInterface;

                while (currentClass != null)
                {
                    try
                    {
                        targetMethod = currentClass.getDeclaredMethod(targetMethodName, parameterTypes);
                        break;
                    }
                    catch (NoSuchMethodException e)
                    {
                        currentClass = currentClass.getSuperclass();
                    }
                }
            }
        }

        if(targetMethod != null)
        {
            return targetMethod;
        }

        throw new NoSuchMethodException("there is no method with the name '" + targetMethodName + "'" +
                " class: " + targetClass.getName());
    }

    public static Object tryToInvokeMethod(Object target, Method method)
    {
        return tryToInvokeMethod(target, method, null);
    }

    public static Object tryToInvokeMethodOfClass(Class target, Method method)
    {
        return tryToInvokeMethodOfClass(target, method, null);
    }

    public static Object tryToInvokeMethodOfClass(Class target, Method method, Object[] args)
    {
        try
        {
            return invokeMethodOfClass(target, method, args);
        }
        catch (Throwable e)
        {
            //do nothing - it's just a try
            return null;
        }
    }

    public static Object invokeMethodOfClass(Class target, Method method)
        throws IllegalAccessException, InstantiationException, InvocationTargetException
    {
        return invokeMethod(target.newInstance(), method, null);
    }

    public static Object invokeMethodOfClass(Class target, Method method, Object... args)
        throws IllegalAccessException, InstantiationException, InvocationTargetException
    {
        return invokeMethod(target.newInstance(), method, args);
    }

    public static Object tryToInvokeMethod(Object target, Method method, Object... args)
    {
        try
        {
            return invokeMethod(target, method, args);
        }
        catch (Throwable t)
        {
            //do nothing - it's just a try
            return null;
        }
    }

    public static Object invokeMethod(Object target, Method method)
        throws InvocationTargetException, IllegalAccessException
    {
        return invokeMethod(target, method, null);
    }

    public static Object invokeMethod(Object target, Method method, Object... args)
        throws InvocationTargetException, IllegalAccessException
    {
        method.setAccessible(true);
        return method.invoke(target, args);
    }

    public static Object getBaseOfPropertyChain(Object baseObject, String propertyChain)
    {
        StringTokenizer tokenizer = new StringTokenizer(propertyChain, ".");

        Object currentBase = baseObject;
        String currentProperty;
        Method currentMethod;

        while(tokenizer.hasMoreTokens())
        {
            currentProperty = tokenizer.nextToken();

            //ignore the last property
            if(!tokenizer.hasMoreTokens())
            {
                break;
            }

            //no is - it's only possible at properties not at bean level
            currentMethod = tryToGetMethod(currentBase.getClass(),
                "get" + currentProperty.substring(0, 1).toUpperCase() +
                    currentProperty.substring(1, currentProperty.length()));
            currentBase = tryToInvokeMethod(currentBase, currentMethod);
        }

        return currentBase;
    }
}
