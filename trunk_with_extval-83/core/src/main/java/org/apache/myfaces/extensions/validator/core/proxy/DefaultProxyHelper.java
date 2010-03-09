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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultProxyHelper implements ProxyHelper
{
    protected final Log logger = LogFactory.getLog(getClass());

    public DefaultProxyHelper()
    {
        if (logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public <T> Class<T> getUnproxiedClass(Class currentClass, Class<T> targetType)
    {
        return (Class<T>)getUnproxiedClass(currentClass);
    }

    public Class getUnproxiedClass(Class currentClass)
    {
        if(isProxiedClass(currentClass))
        {
            return ClassUtils.tryToLoadClassForName(getNameOfClass(currentClass));
        }
        return currentClass;
    }

    public String getNameOfClass(Class proxiedClass)
    {
        if (isProxiedClass(proxiedClass))
        {
            return proxiedClass.getName().substring(0, proxiedClass.getName().indexOf("$"));
        }
        return proxiedClass.getName();
    }

    public String getClassNameOfObject(Object proxiedObject)
    {
        if(proxiedObject != null)
        {
            return getNameOfClass(proxiedObject.getClass());
        }
        return null;
    }

    public boolean isProxiedClass(Class currentClass)
    {
        return currentClass.getName().contains("$$EnhancerByCGLIB$$")
            || currentClass.getName().contains("$$FastClassByCGLIB$$");
    }

    public boolean isProxiedObject(Object proxiedObject)
    {
        return proxiedObject != null && isProxiedClass(proxiedObject.getClass());
    }
}