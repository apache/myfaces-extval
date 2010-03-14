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

import org.apache.myfaces.extensions.validator.core.proxy.ProxyHelper;
import org.apache.myfaces.extensions.validator.core.proxy.DefaultProxyHelper;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;

/**
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ProxyUtils
{
    private static ProxyHelper proxyHelper;

    public static <T> Class<T> getUnproxiedClass(Class currentClass, Class<T> targetType)
    {
        return getProxyHelper().getUnproxiedClass(currentClass, targetType);
    }

    public static Class getUnproxiedClass(Class currentClass)
    {
        return getProxyHelper().getUnproxiedClass(currentClass);
    }

    public static String getClassName(Class proxiedClass)
    {
        return getProxyHelper().getNameOfClass(proxiedClass);
    }

    public static String getClassNameOfObject(Object proxiedObject)
    {
        return getProxyHelper().getClassNameOfObject(proxiedObject);
    }

    public static boolean isProxiedClass(Class currentClass)
    {
        return getProxyHelper().isProxiedClass(currentClass);
    }

    public static boolean isProxiedObject(Object proxiedObject)
    {
        return getProxyHelper().isProxiedObject(proxiedObject);
    }

    private static ProxyHelper getProxyHelper()
    {
        if (proxyHelper == null)
        {
            //workaround for mojarra
            if(!JsfUtils.isApplicationInitialized())
            {
                return new DefaultProxyHelper();
            }

            proxyHelper = createProxyHelper();
        }
        return proxyHelper;
    }

    //don't use the default approach (factory finder) - ProxyHelper is called too often...
    private static ProxyHelper createProxyHelper()
    {
        String customProxyHelperClassName = WebXmlParameter.CUSTOM_PROXY_HELPER;

        ProxyHelper result = null;
        if(customProxyHelperClassName != null && !"".equals(customProxyHelperClassName))
        {
            result = (ProxyHelper)ClassUtils.tryToInstantiateClassForName(customProxyHelperClassName);
        }
        if(result == null)
        {
            result = new DefaultProxyHelper();
        }

        return result;
    }
}
