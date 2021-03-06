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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.jar.Manifest;
import java.util.jar.Attributes;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.net.URL;

/**
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ClassUtils
{
    private static final Logger LOG = Logger.getLogger(ClassUtils.class.getName());

    public static Class tryToLoadClassForName(String name)
    {
        try
        {
            return loadClassForName(name);
        }
        catch (ClassNotFoundException e)
        {
            //do nothing - it's just a try
            return null;
        }
    }
    
    public static Class loadClassForName(String name) throws ClassNotFoundException
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

    public static <T> T tryToInstantiateClass(Class<T> targetClass)
    {
        try
        {
            return targetClass.newInstance();
        }
        catch (Exception e)
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
        catch (Exception e)
        {
            //do nothing - it was just a try
        }
        return null;
    }

    public static Object instantiateClassForName(String className)
        throws ClassNotFoundException, IllegalAccessException, InstantiationException
    {
        return loadClassForName(className).newInstance();
    }

    public static String getJarVersion(Class targetClass)
    {
        String classFilePath = targetClass.getCanonicalName().replace('.', '/') + ".class";
        String manifestFilePath = "/META-INF/MANIFEST.MF";

        String classLocation = targetClass.getResource(targetClass.getSimpleName() + ".class").toString();
        String manifestFileLocation = classLocation
                .substring(0, classLocation.indexOf(classFilePath) - 1) + manifestFilePath;

        try
        {
            return new Manifest(new URL(manifestFileLocation).openStream())
                    .getMainAttributes().getValue(Attributes.Name.IMPLEMENTATION_VERSION);
        }
        catch (Exception e)
        {
            LOG.log(Level.FINEST, "couldn't load version of jar file which contains " + targetClass.getName(), e);
            return null;
        }
    }

    /**
     * Gets the package name.
     * Needed for ClassLoaderTestSuite
     *
     * @param targetClass the the class
     * @return the package name
     */
    public static String getPackageName(Class targetClass)
    {
        Package currentPackage = targetClass.getPackage();

        if(currentPackage != null)
        {
            return currentPackage.getName();
        }

        return targetClass.getName().substring(0, targetClass.getName().lastIndexOf("."));
    }
}
