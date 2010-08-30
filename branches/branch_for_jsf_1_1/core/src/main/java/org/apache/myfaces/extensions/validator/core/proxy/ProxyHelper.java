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

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

/**
 * Allows to have a helper object that deals with proxies, like CGLIB proxies, created for some objects.  There are
 * various locations where we need to know the base class or name of a prpoxy instance.
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface ProxyHelper
{
    /**
     * Returns the Class metadata of the proxy class.  When the parameter isn't a proxy, it returns the parameter value.
     *
     * @param currentClass class of proxy instance or any regular class.
     * @return base class for which the proxy was created.
     */
    Class getUnproxiedClass(Class currentClass);

    /**
     * Returns the Class metadata of the proxy class casted to a certain targetType. There is no check to see if the
     * base class is actually assignable to the targetType.
     * @see ProxyHelper#getUnproxiedClass(java.lang.Class)
     *
     * @param currentClass class of proxy instance or any regular class.
     * @param targetType target class type
     * @param <T> Type declaration for generics.
     * @return base class for which the proxy was created.
     */
    <T> Class<T> getUnproxiedClass(Class currentClass, Class<T> targetType);

    /**
     * Returns the class name of the base class of the proxy.
     * @param proxiedClass class of proxy instance or any regular class.
     * @return class name of base class for which the proxy was created.
     */
    String getNameOfClass(Class proxiedClass);

    /**
     * Returns the class name of the base class of the proxy instance.
     * @param proxiedObject proxy instance or any regular object.
     * @return  class name of base class for which the proxy was created.
     */
    String getClassNameOfObject(Object proxiedObject);

    /**
     * Checks if the class is a proxy class.
     *
     * @param currentClass proxy class to check.
     * @return is it a proxy class or regular class.
     */
    boolean isProxiedClass(Class currentClass);

    /**
     * Checks if this object is a proxy.
     *
     * @param proxiedObject object to check.
     * @return is it a proxy or regular object.
     */
    boolean isProxiedObject(Object proxiedObject);
}
