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
package org.apache.myfaces.extensions.validator.core.validation.message.resolver;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.myfaces.extensions.validator.core.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.
        resolver.mapper.CustomConfiguredValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.
        resolver.mapper.CustomConventionValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.
        resolver.mapper.DefaultModuleValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.
        resolver.mapper.DefaultValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.message.
        resolver.mapper.SimpleValidationStrategyToMsgResolverNameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.util.ClassUtils;

/**
 * @author Gerhard Petracek
 */
//TODO add generic java api (de-/register mapping)
public class DefaultMessageResolverFactory implements
        ClassMappingFactory<ValidationStrategy, MessageResolver>
{
    private static Map<String, String> strategyMessageResolverMapping = 
        new HashMap<String, String>();
    private static List<NameMapper<ValidationStrategy>> nameMapperList = 
        new ArrayList<NameMapper<ValidationStrategy>>();

    static
    {
        nameMapperList
                .add(new CustomConfiguredValidationStrategyToMsgResolverNameMapper());
        nameMapperList
                .add(new CustomConventionValidationStrategyToMsgResolverNameMapper());
        nameMapperList
                .add(new DefaultValidationStrategyToMsgResolverNameMapper());
        nameMapperList
                .add(new DefaultModuleValidationStrategyToMsgResolverNameMapper());
        nameMapperList
                .add(new SimpleValidationStrategyToMsgResolverNameMapper());
    }

    public MessageResolver create(ValidationStrategy validationStrategy)
    {
        String strategyName = validationStrategy.getClass().getName();

        if (strategyMessageResolverMapping.containsKey(strategyName))
        {
            return (MessageResolver) ClassUtils
                    .tryToInstantiateClassForName(strategyMessageResolverMapping
                            .get(strategyName));
        }

        MessageResolver messageResolver;
        String resolverName;
        for (NameMapper<ValidationStrategy> nameMapper : nameMapperList)
        {
            //build convention (ValidationErrorMessageResolver)
            resolverName = nameMapper.createName(validationStrategy);

            //name wasn't mapped
            if(validationStrategy.getClass().getName().equals(resolverName))
            {
                continue;
            }

            messageResolver = (MessageResolver) ClassUtils.tryToInstantiateClassForName(resolverName);

            if (messageResolver != null)
            {
                addMapping(strategyName, resolverName);
                return messageResolver;
            }
        }

        addMapping(strategyName, DefaultValidationErrorMessageResolver.class
                .getName());
        return new DefaultValidationErrorMessageResolver();
    }

    private void addMapping(String strategyName, String messageResolverName)
    {
        synchronized (DefaultMessageResolverFactory.class)
        {
            strategyMessageResolverMapping.put(strategyName,
                    messageResolverName);
        }
        //TODO logging
    }
}
