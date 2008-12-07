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
import org.apache.myfaces.extensions.validator.core.mapper.ClassMappingFactory;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.interceptor.ValidationExceptionInterceptor;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformationKeys;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.AbstractELHelperFactory;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.MetaDataExtractor;
import org.apache.myfaces.extensions.validator.core.metadata.extractor.ComponentMetaDataExtractorFactory;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.MetaDataTransformer;
import org.apache.myfaces.extensions.validator.core.metadata.MetaDataEntry;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.validator.ValidatorException;
import javax.faces.application.FacesMessage;
import java.util.Map;
import java.util.MissingResourceException;


/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.INTERNAL)
public class ExtValUtils
{
    private static final String JAVAX_FACES_REQUIRED = "javax.faces.component.UIInput.REQUIRED";
    private static final String JAVAX_FACES_REQUIRED_DETAIL = "javax.faces.component.UIInput.REQUIRED_detail";

    private static final String JAVAX_FACES_MAXIMUM = "javax.faces.validator.LengthValidator.MAXIMUM";
    private static final String JAVAX_FACES_MAXIMUM_DETAIL = "javax.faces.validator.LengthValidator.MAXIMUM_detail";

    public static ValidationStrategy getValidationStrategyForMetaData(String metaDataKey)
    {
        return ((ClassMappingFactory<Object, ValidationStrategy>) ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, ClassMappingFactory.class))
                .create(metaDataKey);
    }

    public static MetaDataTransformer getMetaDataTransformerForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MetaDataTransformer>) ExtValContext
                    .getContext().getFactoryFinder()
                    .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, ClassMappingFactory.class))
                    .create(validationStrategy);
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

    public static MessageResolver getMessageResolverForValidationStrategy(ValidationStrategy validationStrategy)
    {
        return ((ClassMappingFactory<ValidationStrategy, MessageResolver>)ExtValContext.getContext()
            .getFactoryFinder()
            .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, ClassMappingFactory.class))
            .create(validationStrategy);
    }

    public static ELHelper getELHelper()
    {
        return ExtValContext.getContext().getFactoryFinder()
            .getFactory(FactoryNames.EL_HELPER_FACTORY, AbstractELHelperFactory.class).create();
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
}
