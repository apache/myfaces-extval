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

import org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.factory.AbstractNameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationEntry;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.util.ClassUtils;
import org.apache.myfaces.extensions.validator.util.ProxyUtils;
import org.apache.myfaces.extensions.validator.util.NullValueAwareConcurrentHashMap;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Logger;

/**
 * Factory which creates a MessageResolver for a given ValidationStrategy
 *
 * @since 1.x.1
 */
@ToDo(value = Priority.MEDIUM, description = "add generic java api (de-/register mapping)")
@UsageInformation({UsageCategory.INTERNAL, UsageCategory.CUSTOMIZABLE})
public class DefaultMessageResolverFactory extends AbstractNameMapperAwareFactory<ValidationStrategy>
        implements ClassMappingFactory<ValidationStrategy, MessageResolver>
{
    protected final Logger logger = Logger.getLogger(getClass().getName());

    private Map<String, String> strategyMessageResolverMapping;
    private List<NameMapper<ValidationStrategy>> nameMapperList =
            new CopyOnWriteArrayList<NameMapper<ValidationStrategy>>();

    public DefaultMessageResolverFactory()
    {
        logger.fine(getClass().getName() + " instantiated");
    }

    public MessageResolver create(ValidationStrategy validationStrategy)
    {
        String strategyName = ProxyUtils.getClassName(validationStrategy.getClass());

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
            if (resolverName == null || ProxyUtils.getClassName(validationStrategy.getClass()).equals(resolverName))
            {
                continue;
            }

            messageResolver = (MessageResolver) ClassUtils.tryToInstantiateClassForName(resolverName);

            if (messageResolver != null)
            {
                addMapping(strategyName, resolverName);

                logger.finest(resolverName + " used for " + strategyName);

                return messageResolver;
            }
        }

        addMapping(strategyName, DefaultValidationErrorMessageResolver.class.getName());
        return new DefaultValidationErrorMessageResolver();
    }

    private synchronized void initStaticMappings()
    {
        strategyMessageResolverMapping = new NullValueAwareConcurrentHashMap<String, String>(String.class);

        //setup internal static mappings
        for (StaticConfiguration<String, String> staticConfig :
            ExtValContext.getContext().getStaticConfiguration(
                StaticConfigurationNames.VALIDATION_STRATEGY_TO_MESSAGE_RESOLVER_CONFIG))
        {
            setupMappings(staticConfig.getMapping());
        }
    }

    private void setupMappings(List<StaticConfigurationEntry<String,String>> mappings)
    {
        for(StaticConfigurationEntry<String, String> mapping : mappings)
        {
            addMapping(mapping.getSource(), mapping.getTarget());
        }
    }

    @ToDo(value = Priority.MEDIUM, description = "logging")
    private synchronized void addMapping(String validationStrategyName, String messageResolverName)
    {
        logger.finest("adding static validation strategy to message resolver mapping: "
            + validationStrategyName + " -> " + messageResolverName);

        strategyMessageResolverMapping.put(validationStrategyName, messageResolverName);
    }

    protected List<NameMapper<ValidationStrategy>> getNameMapperList()
    {
        return nameMapperList;
    }
}
