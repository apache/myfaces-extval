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

import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;
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
import org.apache.myfaces.extensions.validator.core.loader.StaticMappingConfigLoader;
import org.apache.myfaces.extensions.validator.core.loader.StaticMappingConfigEntry;
import org.apache.myfaces.extensions.validator.core.loader.StaticMappingConfigLoaderNames;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Factory which creates a MessageResolver for a given ValidationStrategy
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@ToDo(value = Priority.MEDIUM, description = "add generic java api (de-/register mapping)")
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public class DefaultMessageResolverFactory implements
    ClassMappingFactory<ValidationStrategy, MessageResolver>
{
    protected final Log logger = LogFactory.getLog(getClass());

    private static Map<String, String> strategyMessageResolverMapping;
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

    public DefaultMessageResolverFactory()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public MessageResolver create(ValidationStrategy validationStrategy)
    {
        String strategyName = validationStrategy.getClass().getName();

        if (strategyMessageResolverMapping == null)
        {
            initStaticMappings();
        }

        if (strategyMessageResolverMapping.containsKey(strategyName))
        {
            return (MessageResolver) ClassUtils
                .tryToInstantiateClassForName(strategyMessageResolverMapping.get(strategyName));
        }

        MessageResolver messageResolver;
        String resolverName;
        for (NameMapper<ValidationStrategy> nameMapper : nameMapperList)
        {
            //build convention (ValidationErrorMessageResolver)
            resolverName = nameMapper.createName(validationStrategy);

            //name wasn't mapped
            if (resolverName == null || validationStrategy.getClass().getName().equals(resolverName))
            {
                continue;
            }

            messageResolver = (MessageResolver) ClassUtils.tryToInstantiateClassForName(resolverName);

            if (messageResolver != null)
            {
                addMapping(strategyName, resolverName);

                if(logger.isTraceEnabled())
                {
                    logger.trace(resolverName + " used for " + strategyName);
                }

                return messageResolver;
            }
        }

        addMapping(strategyName, DefaultValidationErrorMessageResolver.class.getName());
        return new DefaultValidationErrorMessageResolver();
    }

    private void initStaticMappings()
    {
        synchronized (DefaultMessageResolverFactory.class)
        {
            strategyMessageResolverMapping = new HashMap<String, String>();

            //setup internal static mappings
            for (StaticMappingConfigLoader<String, String> staticMappingConfigLoader :
                ExtValContext.getContext().getStaticMappingConfigLoaders(
                    StaticMappingConfigLoaderNames.VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_CONFIG_LOADER))
            {
                setupStrategyMappings(staticMappingConfigLoader.getMapping());
            }
        }
    }

    private void setupStrategyMappings(List<StaticMappingConfigEntry<String,String>> mappings)
    {
        for(StaticMappingConfigEntry<String, String> mapping : mappings)
        {
            addMapping(mapping.getSource(), mapping.getTarget());
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    private void addMapping(String validationStrategyName, String messageResolverName)
    {
        if(logger.isTraceEnabled())
        {
            logger.trace("adding static validation strategy to message resolver mapping: "
                + validationStrategyName + " -> " + messageResolverName);
        }

        synchronized (DefaultMessageResolverFactory.class)
        {
            strategyMessageResolverMapping.put(validationStrategyName, messageResolverName);
        }
    }
}
