/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.extensions.validator.util;

/**
 * @author Gerhard Petracek
 */
public class ClassUtils
{
    public static Class loadClassForName(String name)
            throws ClassNotFoundException
    {
        try
        {
            // Try WebApp ClassLoader first
            return Class.forName(name, false, // do not initialize for faster startup
                    Thread.currentThread().getContextClassLoader());
        }
        catch (ClassNotFoundException ignore)
        {
            // fallback: Try ClassLoader for ClassUtils (i.e. the myfaces.jar lib)
            return Class.forName(name, false, // do not initialize for faster startup
                    ClassUtils.class.getClassLoader());
        }
    }

    public static Object tryToInstantiateClass(Class targetClass)
    {
        try
        {
            return targetClass.newInstance();
        }
        catch (Throwable t)
        {
            //do nothing - it was just a try
        }
        return null;
    }

    public static Object tryToInstantiateClassForName(String className)
    {
        try
        {
            return instantiateClassForName(className);
        }
        catch (Throwable t)
        {
            //do nothing - it was just a try
        }
        return null;
    }

    public static Object instantiateClassForName(String className)
            throws ClassNotFoundException, IllegalAccessException,
            InstantiationException
    {
        return loadClassForName(className).newInstance();
    }
}
