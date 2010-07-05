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
package org.apache.myfaces.extensions.validator.core.proxy;

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

import java.util.logging.Logger;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultProxyHelper implements ProxyHelper
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    public DefaultProxyHelper()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    public <T> Class<T> getUnproxiedClass(Class currentClass, Class<T> targetType)
    {
        return (Class<T>)getUnproxiedClass(currentClass);
    }

    public Class getUnproxiedClass(Class currentClass)
    {
        if(isProxiedClass(currentClass))
        {
            return currentClass.getSuperclass();
        }
        return currentClass;
    }

    public String getNameOfClass(Class currentClass)
    {
        if (isProxiedClass(currentClass))
        {
            return currentClass.getSuperclass().getName();
        }
        return currentClass.getName();
    }

    public String getClassNameOfObject(Object object)
    {
        if(object != null)
        {
            return getNameOfClass(object.getClass());
        }
        return null;
    }

    public boolean isProxiedClass(Class currentClass)
    {
        return currentClass.getName().contains("$$EnhancerByCGLIB$$") ||
            currentClass.getName().contains("$$FastClassByCGLIB$$") ||
            currentClass.getName().contains("_$$_javassist");
    }

    public boolean isProxiedObject(Object proxiedObject)
    {
        return proxiedObject != null && isProxiedClass(proxiedObject.getClass());
    }
}
