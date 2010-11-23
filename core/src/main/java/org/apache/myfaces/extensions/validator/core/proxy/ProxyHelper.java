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
 * Pluggable helper which handles proxied instances correctly.
 * (The default implementation supports cglib and javassist
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface ProxyHelper
{
    /**
     * Returns the original class for the given class which might be the class of a proxied instance.
     *
     * @param currentClass class of proxy instance or any regular class.
     * @return the class of the original (unproxied) instance or the parameter itself
     */
    Class getUnproxiedClass(Class currentClass);

    /**
     * In addition to ProxyHelper#getUnproxiedClass(java.lang.Class) it casts to the given type.
     * @see ProxyHelper#getUnproxiedClass(java.lang.Class)
     *
     * @param currentClass class of proxy instance or any regular class.
     * @param targetType target class type
     * @param <T> Type declaration for generics.
     * @return the class of the original (unproxied) instance or the parameter itself
     */
    <T> Class<T> getUnproxiedClass(Class currentClass, Class<T> targetType);

    /**
     * Returns the original fully qualified class-name for
     * the given class which might be the class of a proxied instance.
     *
     * @param proxiedClass class of proxy instance or any regular class.
     * @return the class-name of the original (unproxied) instance or the name of the parameter itself
     */
    String getNameOfClass(Class proxiedClass);

    /**
     * Returns the original fully qualified class-name for
     * the given object which might be a proxied instance.
     *
     * @param proxiedObject proxy instance or any regular object.
     * @return the class-name of the original (unproxied) instance or the name of the parameter itself
     */
    String getClassNameOfObject(Object proxiedObject);

    /**
     * Checks if the given class is a class of a proxied instance.
     *
     * @param currentClass proxy class to check.
     * @return true if the given class is a class of a proxied instance
     */
    boolean isProxiedClass(Class currentClass);

    /**
     * Checks if the given instance is proxied.
     *
     * @param proxiedObject object to check.
     * @return true if the given object is a proxied instance
     */
    boolean isProxiedObject(Object proxiedObject);
}
