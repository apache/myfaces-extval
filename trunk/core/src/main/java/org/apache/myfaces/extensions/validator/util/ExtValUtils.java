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
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.MessageResolver;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractor;
import org.apache.myfaces.extensions.validator.core.validation.parameter.ValidationParameterExtractorFactory;
import org.apache.myfaces.extensions.validator.core.factory.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.PropertyValidationInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.AbstractELHelperFactory;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfiguration;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationEntry;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.factory.FacesMessageFactory;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import java.util.Map;
import java.util.HashMap;
import java.util.MissingResourceException;
import java.util.List;
import java.util.ArrayList;
import java.lang.annotation.Annotation;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValUtils
{
    private static final Log LOGGER = LogFactory.getLog(ExtValUtils.class);

    private static final String JAVAX_FACES_REQUIRED = "javax.faces.component.UIInput.REQUIRED";
    private static final String JAVAX_FACES_REQUIRED_DETAIL = "javax.faces.component.UIInput.REQUIRED_detail";

    private static final String JAVAX_FACES_MAXIMUM = "javax.faces.validator.LengthValidator.MAXIMUM";
    private static final String JAVAX_FACES_MAXIMUM_DETAIL = "javax.faces.validator.LengthValidator.MAXIMUM_detail";

    public static ValidationStrategy getValidationStrategyForMetaData(String metaDataKey)
    {
        return ((ClassMappingFactory<String, ValidationStrategy>) ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, ClassMappingFactory.class))
                .create(metaDataKey);
    }

    public static void registerMetaDataToValidationStrategyNameMapper(
            NameMapper<String> metaDataToValidationStrategyNameMapper)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, NameMapperAwareFactory.class))
                .register(metaDataToValidationStrategyNameMapper);
    }

    public static void deregisterMetaDataToValidationStrategyNameMapper(
            Class<? extends NameMapper> metaDataToValidationStrategyNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, NameMapperAwareFactory.class))
                .deregister(metaDataToValidationStrategyNameMapperClass);
    }

    public static void denyMetaDataToValidationStrategyNameMapper(
            Class<? extends NameMapper> metaDataToValidationStrategyNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, NameMapperAwareFactory.class))
                .deny(metaDataToValidationStrategyNameMapperClass);
    }

    public static MetaDataTransformer getMetaDataTransformerForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MetaDataTransformer>) ExtValContext
                    .getContext().getFactoryFinder()
                    .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, ClassMappingFactory.class))
                    .create(validationStrategy);
    }

    public static void registerValidationStrategyToMetaDataTransformerNameMapper(
            NameMapper<ValidationStrategy> validationStrategyToMetaDataTransformerNameMapper)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, NameMapperAwareFactory.class))
                .register(validationStrategyToMetaDataTransformerNameMapper);
    }

    public static void deregisterValidationStrategyToMetaDataTransformerNameMapper(
            Class<? extends NameMapper> validationStrategyToMetaDataTransformerNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, NameMapperAwareFactory.class))
                .deregister(validationStrategyToMetaDataTransformerNameMapperClass);
    }

    public static void denyValidationStrategyToMetaDataTransformerNameMapper(
            Class<? extends NameMapper> validationStrategyToMetaDataTransformerNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, NameMapperAwareFactory.class))
                .deny(validationStrategyToMetaDataTransformerNameMapperClass);
    }

    public static MetaDataExtractor getComponentMetaDataExtractor()
    {
            return ExtValContext.getContext().getFactoryFinder()
                .getFactory(FactoryNames.COMPONENT_META_DATA_EXTRACTOR_FACTORY, ComponentMetaDataExtractorFactory.class)
                .create();
    }

    public static void configureComponentWithMetaData(FacesContext facesContext,
                                                      UIComponent uiComponent,
                                                      Map<String, Object> metaData)
    {
        for(ComponentInitializer componentInitializer : ExtValContext.getContext().getComponentInitializers())
        {
            componentInitializer.configureComponent(facesContext, uiComponent, metaData);
        }
    }

    public static boolean executeAfterThrowingInterceptors(UIComponent uiComponent,
                                                        MetaDataEntry metaDataEntry,
                                                        Object convertedObject,
                                                        ValidatorException validatorException,
                                                        ValidationStrategy validatorExceptionSource)
    {
        boolean result = true;

        for(ValidationExceptionInterceptor validationExceptionInterceptor : ExtValContext.getContext()
                .getValidationExceptionInterceptors())
        {
            if(!validationExceptionInterceptor.afterThrowing(
                            uiComponent, metaDataEntry, convertedObject, validatorException, validatorExceptionSource))
            {
                result = false;
            }
        }

        return result;
    }

    public static MetaDataExtractor createInterceptedMetaDataExtractor(final MetaDataExtractor metaDataExtractor)
    {
        return new MetaDataExtractor()
        {
            public PropertyInformation extract(FacesContext facesContext, Object object)
            {
                PropertyInformation result = metaDataExtractor.extract(facesContext, object);
                for(MetaDataExtractionInterceptor metaDataExtractionInterceptor :
                        ExtValContext.getContext().getMetaDataExtractionInterceptors())
                {
                    metaDataExtractionInterceptor.afterExtracting(result);
                }
                return result;
            }
        };
    }

    public static MessageResolver getMessageResolverForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MessageResolver>)ExtValContext.getContext()
            .getFactoryFinder()
            .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, ClassMappingFactory.class))
            .create(validationStrategy);
    }

    public static void registerValidationStrategyToMessageResolverNameMapper(
            NameMapper<ValidationStrategy> validationStrategyToMsgResolverNameMapper)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class))
                .register(validationStrategyToMsgResolverNameMapper);
    }

    public static void deregisterValidationStrategyToMessageResolverNameMapper(
            Class<? extends NameMapper> validationStrategyToMessageResolverNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class))
                .deregister(validationStrategyToMessageResolverNameMapperClass);
    }

    public static void denyValidationStrategyToMessageResolverNameMapper(
            Class<? extends NameMapper> validationStrategyToMessageResolverNameMapperClass)
    {
        (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class))
                .deny(validationStrategyToMessageResolverNameMapperClass);
    }

    public static ELHelper getELHelper()
    {
        return ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.EL_HELPER_FACTORY, AbstractELHelperFactory.class).create();
    }

    public static FacesMessage createFacesMessage(String summary, String detail)
    {
        return createFacesMessage(FacesMessage.SEVERITY_ERROR, summary, detail);
    }

    public static FacesMessage createFacesMessage(FacesMessage.Severity severity, String summary, String detail)
    {
        return ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class)
                .create(severity, summary, detail);
    }

    public static FacesMessage convertFacesMessage(FacesMessage facesMessage)
    {
        return ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.FACES_MESSAGE_FACTORY, FacesMessageFactory.class)
                .convert(facesMessage);
    }

    public static PropertyDetails createPropertyDetailsForNewTarget(MetaDataEntry metaDataEntry,
                                                                                  String targetExpression)
    {
        Object baseObject;
        if(ExtValUtils.getELHelper().isELTermWellFormed(targetExpression))
        {
            ValueBindingExpression vbe = new ValueBindingExpression(targetExpression);

            String expression = vbe.getExpressionString();
            baseObject = ExtValUtils.getELHelper()
                    .getValueOfExpression(FacesContext.getCurrentInstance(), vbe.getBaseExpression());
            return new PropertyDetails(
                expression.substring(2, expression.length() - 1), baseObject, vbe.getProperty());
        }

        PropertyDetails original = metaDataEntry.getProperty(
            PropertyInformationKeys.PROPERTY_DETAILS, PropertyDetails.class);

        String newBaseKey = original.getKey().substring(0, original.getKey().lastIndexOf(".") + 1);
        String newKey = newBaseKey + targetExpression;

        baseObject = ReflectionUtils.getBaseOfPropertyChain(original.getBaseObject(), targetExpression);
        return new PropertyDetails(
            newKey, baseObject, targetExpression.substring(targetExpression.lastIndexOf(".") + 1,
            targetExpression.length()));
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void tryToPlaceLabel(FacesMessage facesMessage, String label, int index)
    {
        if(facesMessage.getSummary() != null && facesMessage.getSummary().contains("{" + index + "}"))
        {
            facesMessage.setSummary(facesMessage.getSummary().replace("{" + index + "}", label));
        }

        if(facesMessage.getDetail() != null && facesMessage.getDetail().contains("{" + index + "}"))
        {
            facesMessage.setDetail(facesMessage.getDetail().replace("{" + index + "}", label));
        }
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void replaceWithDefaultMaximumMessage(FacesMessage facesMessage, int maxLength)
    {
        String facesRequiredMessage = JsfUtils.getDefaultFacesMessageBundle().getString(JAVAX_FACES_MAXIMUM);
        String facesRequiredMessageDetail = facesRequiredMessage;

        //use try/catch for easier sync between trunk/branch
        try
        {
            if(JsfUtils.getDefaultFacesMessageBundle().getString(JAVAX_FACES_MAXIMUM_DETAIL) != null)
            {
                facesRequiredMessageDetail = JsfUtils
                        .getDefaultFacesMessageBundle().getString(JAVAX_FACES_MAXIMUM_DETAIL);
            }
        }
        catch (MissingResourceException missingResourceException)
        {
            //jsf 1.2 doesn't have a detail message
        }

        facesRequiredMessage = facesRequiredMessage.replace("{0}", "" + maxLength);
        facesRequiredMessageDetail = facesRequiredMessageDetail.replace("{0}", "" + maxLength);

        facesMessage.setSummary(facesRequiredMessage);
        facesMessage.setDetail(facesRequiredMessageDetail);
    }

    @UsageInformation(UsageCategory.INTERNAL)
    public static void replaceWithDefaultRequiredMessage(FacesMessage facesMessage)
    {
        String facesRequiredMessage = JsfUtils.getDefaultFacesMessageBundle().getString(JAVAX_FACES_REQUIRED);
        String facesRequiredMessageDetail = facesRequiredMessage;

        //use try/catch for easier sync between trunk/branch
        try
        {
            if(JsfUtils.getDefaultFacesMessageBundle().getString(JAVAX_FACES_REQUIRED_DETAIL) != null)
            {
                facesRequiredMessageDetail = JsfUtils
                        .getDefaultFacesMessageBundle().getString(JAVAX_FACES_REQUIRED_DETAIL);
            }
        }
        catch (MissingResourceException missingResourceException)
        {
            //jsf 1.2 doesn't have a detail message
        }

        facesMessage.setSummary(facesRequiredMessage);
        facesMessage.setDetail(facesRequiredMessageDetail);
    }

    public static boolean isSkipableValidationStrategy(Class<? extends ValidationStrategy> targetClass)
    {
        for(Class currentClass : ExtValUtils.getSkipValidationSupportClassList())
        {
            if(ExtValUtils.isSkipValidationSupported(currentClass, targetClass))
            {
                return true;
            }
        }

        return false;
    }

    public static boolean processMetaDataEntryAfterSkipValidation(
            Class<? extends ValidationStrategy> targetClass, MetaDataEntry entry)
    {
        return ExtValUtils.isSkipableValidationStrategy(targetClass) &&
                Boolean.TRUE.equals(entry.getProperty(PropertyInformationKeys.SKIP_VALIDATION, Boolean.class));
    }

    public static List<Class> getSkipValidationSupportClassList()
    {
        List<StaticConfiguration<String, String>> staticConfigurationList = ExtValContext.getContext()
                .getStaticConfiguration(StaticConfigurationNames.SKIP_VALIDATION_SUPPORT_CONFIG);

        List<Class> markerList = new ArrayList<Class>();

        Class currentClass;
        for(StaticConfiguration<String, String> currentEntry : staticConfigurationList)
        {
            for(StaticConfigurationEntry<String, String> currentConfigurationEntry : currentEntry.getMapping())
            {
                currentClass = ClassUtils.tryToLoadClassForName(currentConfigurationEntry.getTarget());

                if(currentClass != null)
                {
                    markerList.add(currentClass);
                }
                else
                {
                    if(LOGGER.isWarnEnabled())
                    {
                        LOGGER.warn("configuration entry provides an invalid entry: "
                                + currentConfigurationEntry.getTarget());
                    }
                }
            }
        }

        return markerList;
    }

    @SuppressWarnings({"unchecked"})
    public static boolean isSkipValidationSupported(Class currentClass, Class targetClass)
    {
        if(currentClass.isAnnotation())
        {
            if(targetClass.isAnnotationPresent(currentClass))
            {
                return true;
            }
        }
        else
        {
            if(currentClass.isAssignableFrom(targetClass))
            {
                return true;
            }
        }

        return false;
    }

    public static ValidationParameterExtractor getValidationParameterExtractor()
    {
        return ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.VALIDATION_PARAMETER_EXTRACTOR_FACTORY, ValidationParameterExtractorFactory.class)
            .create();
    }

    public static boolean executeLocalBeforeValidationInterceptors(FacesContext facesContext,
                                                                   UIComponent uiComponent,
                                                                   Object convertedObject,
                                                                   String propertyKey,
                                                                   Object properties,
                                                                   Annotation annotation)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();
        List<PropertyValidationInterceptor> propertyValidationInterceptors = getValidationParameterExtractor().extract(
                annotation, PropertyValidationInterceptor.class, PropertyValidationInterceptor.class);
        boolean result = true;

        if(properties != null)
        {
            propertyMap.put(propertyKey, properties);
            propertyMap.put(Annotation.class.getName(), annotation);
        }

        for(PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            if(!propertyValidationInterceptor.beforeValidation(facesContext, uiComponent, convertedObject, propertyMap))
            {
                result = false;
            }
        }

        return result;
    }

    public static void executeLocalAfterValidationInterceptors(FacesContext facesContext,
                                                               UIComponent uiComponent,
                                                               Object convertedObject,
                                                               String propertyKey,
                                                               Object properties,
                                                               Annotation annotation)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();
        List<PropertyValidationInterceptor> propertyValidationInterceptors = getValidationParameterExtractor().extract(
                annotation, PropertyValidationInterceptor.class, PropertyValidationInterceptor.class);

        if(properties != null)
        {
            propertyMap.put(propertyKey, properties);
            propertyMap.put(Annotation.class.getName(), annotation);
        }

        for(PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            propertyValidationInterceptor.afterValidation(facesContext, uiComponent, convertedObject, propertyMap);
        }
    }

    public static boolean executeGlobalBeforeValidationInterceptors(FacesContext facesContext,
                                                                    UIComponent uiComponent,
                                                                    Object convertedObject,
                                                                    String propertyKey,
                                                                    Object properties)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();
        boolean result = true;

        if(properties != null)
        {
            propertyMap.put(propertyKey, properties);
        }

        List<PropertyValidationInterceptor> propertyValidationInterceptors =
                ExtValContext.getContext().getPropertyValidationInterceptors();

        for(PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            if(!propertyValidationInterceptor.beforeValidation(facesContext, uiComponent, convertedObject, propertyMap))
            {
                result = false;
            }
        }

        return result;
    }

    public static void executeGlobalAfterValidationInterceptors(FacesContext facesContext,
                                                                UIComponent uiComponent,
                                                                Object convertedObject,
                                                                String propertyKey,
                                                                Object properties)
    {
        Map<String, Object> propertyMap = new HashMap<String, Object>();

        if(properties != null)
        {
            propertyMap.put(propertyKey, properties);
        }

        List<PropertyValidationInterceptor> propertyValidationInterceptors =
                ExtValContext.getContext().getPropertyValidationInterceptors();

        for(PropertyValidationInterceptor propertyValidationInterceptor : propertyValidationInterceptors)
        {
            propertyValidationInterceptor.afterValidation(facesContext, uiComponent, convertedObject, propertyMap);
        }
    }
}
