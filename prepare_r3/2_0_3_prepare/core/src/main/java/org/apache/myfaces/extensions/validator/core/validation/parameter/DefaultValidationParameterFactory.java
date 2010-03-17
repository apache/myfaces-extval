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
package org.apache.myfaces.extensions.validator.core.validation.parameter;

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationEntry;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * maps internal parameters to optionally available custom parameters
 *
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultValidationParameterFactory implements ClassMappingFactory<Class, Class>
{
    private Map<Class, Class> parameterMapping = new HashMap<Class, Class>();

    public Class create(Class source)
    {
        if (this.parameterMapping.containsKey(source))
        {
            return this.parameterMapping.get(source);
        }

        Class result;

        //it's important to look for the static configs first - global parameters are used internally
        result = tryToFindStaticConfig(source);

        if (result == null)
        {
            result = tryToFindGlobalParameter(source);
        }

        if (result == null)
        {
            result = source;
        }

        cacheMapping(source, result);
        return result;
    }

    private Class tryToFindGlobalParameter(Class source)
    {
        Object target = ExtValContext.getContext().getGlobalProperty(source.getName());

        if (target instanceof Class)
        {
            return (Class) target;
        }
        return null;
    }

    private Class tryToFindStaticConfig(Class source)
    {
        Class result = null;
        for (StaticConfiguration<String, String> config : ExtValContext.getContext()
                .getStaticConfiguration(StaticConfigurationNames.VALIDATION_PARAMETER_CONFIG))
        {
            result = tryToMap(source, config.getMapping());

            if (result != null)
            {
                break;
            }
        }

        return result;
    }

    private Class tryToMap(Class source, List<StaticConfigurationEntry<String, String>> mapping)
    {
        Class target = null;

        for (StaticConfigurationEntry<String, String> entry : mapping)
        {
            if (source.getName().equals(entry.getSource()))
            {
                target = ClassUtils.tryToLoadClassForName(entry.getTarget());

                if (target != null)
                {
                    break;
                }
            }
        }

        return target;
    }

    private void cacheMapping(Class source, Class target)
    {
        this.parameterMapping.put(source, target);
    }
}
